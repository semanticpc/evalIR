context('Parsing Run and Qrel Files ')
test_qrelsFile <- system.file('data/tests/toy.adhoc.qrels', 
                         package='evalIR',mustWork=T)

test_runFile1 <- system.file('data/tests/toy.adhoc.res',
                        package='evalIR', mustWork=T)
test_runFile2 <- system.file('data/tests/toy1.adhoc.res', 
                        package='evalIR', mustWork=T)

test_that("Read Qrels: Ad-hoc", {
  
  qrels <- read.qrels(test_qrelsFile, type='adhoc')
  
  nonrelDocs301 <- c('DocA','DocB','DocF', 'DocG', 'DocH', 'DocI', 'DocN','Docp')
  relDocs301 <- c('DocC', 'DocD', 'DocE', 'DocJ', 'DocK', 'DocL', 'DocM','Doco')
  expect_equal(sort(qrels$getQueries()), c(301, 302, 303))
  expect_equal(sort(names(qrels$getGrades(301))), relDocs301)
  expect_equal(length(qrels$getGrades(303)), 0, 
               info="Check when no relevant documents exists in qrels")
  
  expect_equal(sort(qrels$getNonRelDocs(301)), nonrelDocs301)
  
  grades <- c(1,0,1,1)
  names(grades) <- c('DocA','DocB','DocC','DocD')
  qrels$setGrades(303, names(grades), grades)
  expect_equal(sort(names(qrels$getGrades(303))), names(grades), 
               info="checking if setGrades works")
  
  run <- c('DocC', 'DocB', 'DocX')
  expect_equal(qrels$judgeQuery(303, run), c(DocC=1, DocB=0, DocX=0) )
  
})

test_that("Read Qrels: Diversity", {
  test_qrelsFile <- system.file('data/tests/toy.div.qrels', 
                                package='evalIR',mustWork=T)
  qrels <- read.qrels(test_qrelsFile, type='diversity')
  
  nonrelDocs1 <- c('d','i','j')
  mat1 <- matrix(nrow=7, ncol=5)
  mat1[1,] <- c(0, 1, 0, 1, 0)
  mat1[2,] <- c(0, 1, 0, 0, 0)
  mat1[3,] <- c(0, 1, 0, 0, 0)
  mat1[4,] <- c(1, 0, 0, 0, 1)
  mat1[5,] <- c(1, 0, 0, 0, 0)
  mat1[6,] <- c(0, 0, 1, 0, 0)
  mat1[7,] <- c(1, 0, 0, 0, 0)
  rownames(mat1) <- c('a','b','c','e','f','g','h')         
  colnames(mat1) <- c(1,2,3,4,6)
  
  expect_equal(sort(qrels$getQueries()), c(1, 2, 3))
  expect_equal(qrels$getMatrix(1), mat1)
  expect_equal(length(qrels$getMatrix(3)), 0, 
               info="Check when no relevant documents exists in qrels")
  
  expect_equal(sort(qrels$getNonRelDocs(1)), nonrelDocs1)
  
  run <- c('a', 'b', 'i', 'z', 'j')
  judge_mat <- matrix(0, nrow=5, ncol=5)
  judge_mat[1,] <- c(0, 1, 0, 1, 0)
  judge_mat[2,] <- c(0, 1, 0, 0, 0)
  rownames(judge_mat) <- run
  colnames(judge_mat) <- c(1,2,3,4,6)
  expect_equal(qrels$judgeQuery(1, run), judge_mat )
  
})


test_that("Read Qrels: TripletQrels", {
  test_qrelsFile <- system.file('data/tests/toy.triplet.qrels', 
                                package='evalIR',mustWork=T)
  
  qrels <- read.qrels(test_qrelsFile, type='triplets')
  
  #Checking getQueries
  expect_equal(sort(qrels$getQueries()), c(1, 3))
  
  # Checking Preference Counts
  docs <- c('a', 'b', 'c', 'd','e','f','g','h','i','j')
  pref_counts <- c(a=5, b=4, c=2, d=0, e=9, f=5, g=3, h=2, i=0, j=0)
  expect_equal(qrels$getPrefCount(1, docs),  pref_counts)
  pref_counts <- rep(0,10)
  names(pref_counts) <- docs
  expect_equal(qrels$getPrefCount(3, docs),  pref_counts)
  
  # Checking Apprearnce Counts
  app_counts <- c(a=5, b=10, c=10, d=0, e=10, f=10, g=5, h=10, i=0, j=0)
  expect_equal(qrels$getApperanceCount(1, docs),  app_counts)
  app_counts <- c(a=0, b=10, c=2, d=0, e=2, f=2, g=2, h=2, i=0, j=0)
  names(app_counts) <- docs
  expect_equal(qrels$getApperanceCount(3, docs),  app_counts)
  
  # Checing getDocuments
  prf_docs <- c('a', 'b', 'c','e','f','g','h')
  expect_equal(qrels$getDocuments(1),  prf_docs)
  
  
  # Checking Conditional Preference Counts
  cond_doc <- rep('a', 10)
  cond_pref_counts <- c(a=0, b=1, c=0, d=0, e=5, f=4, g=3, h=2, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts)
  cond_doc <- rep('g', 10)
  cond_pref_counts <- c(a=5, b=3, c=2, d=0, e=4, f=1, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts)
  cond_doc <- rep('x', 10)
  cond_pref_counts <- c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts)
  
  cond_doc <- rep('a', 10)
  cond_pref_counts <- rep(0,10)
  names(cond_pref_counts) <- docs
  expect_equal(qrels$getCondPrefCount(3, cond_doc, docs),  cond_pref_counts)
  
  # Checking Conditional Apprearnce Counts
  cond_doc <- rep('a', 10)
  cond_app_counts <- c(a=0, b=5, c=5, d=0, e=5, f=5, g=5, h=5, i=0, j=0)
  expect_equal(qrels$getCondApperanceCount(1, cond_doc, docs),  cond_app_counts)
  
  cond_doc <- rep('x', 10)
  cond_app_counts <- c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondApperanceCount(1, cond_doc, docs),  cond_app_counts)
  expect_equal(qrels$getCondApperanceCount(3, cond_doc, docs),  cond_app_counts)
  
})

test_that("TREC Read Run: getRunids and getQueries", {
  runs <- read.runs(c(test_runFile1, test_runFile2))
  # Testing getQueries and getRunids
  expect_equal(sort(runs$getQueries()), c(301, 302, 303))
  expect_equal(sort(runs$getRunids()), c('toy.adhoc.res', 'toy1.adhoc.res'))
  runs <- read.runs(c(test_runFile1, test_runFile2), runids=c('run1', 'run2'))
  expect_equal(sort(runs$getRunids()), c('run1', 'run2'))
  
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx')
  
  # Testing the limit option
  runs <- read.runs(c(test_runFile1, test_runFile2), runids=c('run1', 'run2'), limit=5)
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='rankSort')
  expect_equal(test_run1RankingA, run1RankingA)
  
})  




test_that("Read Qrels: SimulateTriplets", {
  test_qrelsFile <- system.file('data/tests/toy.div.qrels', 
                                package='evalIR',mustWork=T)
  
  qrels <- read.qrels(test_qrelsFile, type='simulateTriplets')
  expect_equal(sort(qrels$getQueries()), c(1, 2, 3))
  
  params <- list(simulateType='subtopic')
  qrels <- read.qrels(test_qrelsFile, type='simulateTriplets', 
                      opts=params)
  
  
  pref_counts <- c(a=4, e=4)
  expect_more_than(qrels$getPrefCount(1, c('a')), 4)
  expect_more_than(qrels$getPrefCount(1, c('e')), 4)

  params <- list(simulateType='subtopic', resolveTies='alphabetical')
  qrels <- read.qrels(test_qrelsFile, type='simulateTriplets', 
                      opts=params)
  
  # Checking Preference Counts
  docs <- c('a', 'b', 'c', 'd','e','f','g','h','i','j')
  pref_counts <- c(a=6, b=4, c=3, d=0, e=5, f=2, g=1, h=0, i=0, j=0)
  expect_equal(qrels$getPrefCount(1, docs),  pref_counts )
  pref_counts <- rep(0,10)
  names(pref_counts) <- docs
  expect_equal(qrels$getPrefCount(3, docs),  pref_counts )
  
  # Checking Apprearnce Counts
  app_counts <- c(a=6, b=6, c=6, d=0, e=6, f=6, g=6, h=6, i=0, j=0)
  expect_equal(qrels$getApperanceCount(1, docs),  app_counts)
  app_counts <- rep(0,10)
  names(app_counts) <- docs
  expect_equal(qrels$getApperanceCount(3, docs),  app_counts)
  
  
  
  # Checking Conditional Preference Counts
  cond_doc <- rep('a', 10)
  cond_pref_counts <- c(a=0, b=1, c=0, d=0, e=5, f=4, g=3, h=2, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts )
  cond_doc <- rep('g', 10)
  cond_pref_counts <- c(a=5, b=3, c=2, d=0, e=4, f=1, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts )
  cond_doc <- rep('x', 10)
  cond_pref_counts <- c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondPrefCount(1, cond_doc, docs),  cond_pref_counts )
  
  cond_doc <- rep('a', 10)
  cond_pref_counts <- rep(0,10)
  names(cond_pref_counts) <- docs
  expect_equal(qrels$getCondPrefCount(3, cond_doc, docs),  cond_pref_counts )
  
  # Checking Conditional Apprearnce Counts
  cond_doc <- rep('a', 10)
  cond_app_counts <- c(a=0, b=5, c=5, d=0, e=5, f=5, g=5, h=5, i=0, j=0)
  expect_equal(qrels$getCondApperanceCount(1, cond_doc, docs),  cond_app_counts )

  cond_doc <- rep('x', 10)
  cond_app_counts <- c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0)
  expect_equal(qrels$getCondApperanceCount(1, cond_doc, docs),  cond_app_counts )
  expect_equal(qrels$getCondApperanceCount(3, cond_doc, docs),  cond_app_counts )
  
})

test_that("TREC Read Run: getRunids and getQueries", {
  runs <- read.runs(c(test_runFile1, test_runFile2))
  # Testing getQueries and getRunids
  expect_equal(sort(runs$getQueries()), c(301, 302, 303))
  expect_equal(sort(runs$getRunids()), c('toy.adhoc.res', 'toy1.adhoc.res'))
  runs <- read.runs(c(test_runFile1, test_runFile2), runids=c('run1', 'run2'))
  expect_equal(sort(runs$getRunids()), c('run1', 'run2'))
  
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx')
  
  # Testing the limit option
  runs <- read.runs(c(test_runFile1, test_runFile2), runids=c('run1', 'run2'), limit=5)
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='rankSort')
  expect_equal(test_run1RankingA, run1RankingA)
  
})  

test_that("TREC Read Run: getRankMatrix and runSort Methods", {

  runs <- read.runs(c(test_runFile1, test_runFile2),runids=c('run1', 'run2'))
  # Testing getRankMatrix using the runSort Method (Rank Sort)
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx','DocA','DocE','DocB',
                      'DocI','DR303','FR00019','FR00021','DocC','DocD',
                      'FR9001','FR90132','FR90146','FR947','FR904','FR905')
  
  run1RankingB <-  c('FT924-4737', 'FT924-5353', 'FT924-8024', 'FT924-10652', 
                       'FT924-10987', 'FT931-5665', 'FT931-7060', 'FT931-11085', 
                       'FT931-13033', 'FT931-14151')
  
  run2RankingA <- c('DocM','FR9','FR94','DocA','FR90132',
                      'FR9001','FR905','FR00019','DocC','DocD',
                      'DocI','DocE','DocB','DR303','FR00021')
  
  run2RankingB <-  c('DocN','Docx','Doc1','Doc2','Doc3','Doc4',
                       'Doc5','Doc6','Doc7','Doc8','Doc9','Doc10')
  
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='rankSort')
  test_run1RankingB <- runSort(runs,  303, 'run1', sort_type='rankSort')
  test_run2RankingA <- runSort(runs,  301, 'run2', sort_type='rankSort')
  test_run2RankingB <- runSort(runs,  303, 'run2', sort_type='rankSort')
  
  expect_equal(test_run1RankingA, run1RankingA)
  expect_equal(test_run1RankingB, run1RankingB)
  expect_equal(test_run2RankingA, run2RankingA)  
  expect_equal(test_run2RankingB, run2RankingB)
  
  
})
  
test_that("TREC Read Run: getScorekMatrix and runSort Methods", {
  
  # Testing getScoreMatrix using runSort (TREC Sort)
  runs <- read.runs(c(test_runFile1, test_runFile2), runids=c('run1', 'run2'), limit=10)
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='trecSort')
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx','DocE','DocB','DocA',
                    'DocI','DR303')
  expect_equal(test_run1RankingA, run1RankingA)
})









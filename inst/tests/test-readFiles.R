context('Parsing Run and Qrel Files ')
qrelsFile <- system.file('sampledata/test_files/toy.adhoc.qrels', 
                         package='evalIR',mustWork=T)

runFile1 <- system.file('sampledata/test_files/toy.adhoc.res',
                        package='evalIR', mustWork=T)
runFile2 <- system.file('sampledata/test_files/toy1.adhoc.res', 
                        package='evalIR', mustWork=T)

test_that("Read Qrels: Ad-hoc", {
  
  qrels <- read.qrels(qrelsFile, type='adhoc')
  
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



test_that("TREC Read Run: getRunids and getQueries", {

  
  runs <- read.runs(c(runFile1, runFile2))
  # Testing getQueries and getRunids
  expect_equal(sort(runs$getQueries()), c(301, 302, 303))
  expect_equal(sort(runs$getRunids()), c('toy.adhoc.res', 'toy1.adhoc.res'))
  runs <- read.runs(c(runFile1, runFile2), runids=c('run1', 'run2'))
  expect_equal(sort(runs$getRunids()), c('run1', 'run2'))
  
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx')
  
  # Testing the limit option
  runs <- read.runs(c(runFile1, runFile2), runids=c('run1', 'run2'), limit=5)
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='rankSort')
  expect_equal(test_run1RankingA, run1RankingA)
  
})  


test_that("TREC Read Run: getRankMatrix and runSort Methods", {

  runs <- read.runs(c(runFile1, runFile2),runids=c('run1', 'run2'))
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
  runs <- read.runs(c(runFile1, runFile2), runids=c('run1', 'run2'), limit=10)
  test_run1RankingA <- runSort(runs,  301, 'run1', sort_type='trecSort')
  run1RankingA <- c('DocN','DocM','FR9','FR94','Docx','DocE','DocB','DocA',
                    'DocI','DR303')
  expect_equal(test_run1RankingA, run1RankingA)
})





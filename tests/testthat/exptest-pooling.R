context("Pooling Techniques")

test_qrelsFile <- system.file('data/tests/toy.adhoc.qrels', 
                              package='evalIR',mustWork=T)

test_runFile1 <- system.file('data/tests/toy.adhoc.res',
                             package='evalIR', mustWork=T)
test_runFile2 <- system.file('data/tests/toy1.adhoc.res', 
                             package='evalIR', mustWork=T)

test_that("qrels.createSubset",{
  
  queries <- c(301, 301, 302, 302)
  docs <- c('DocM', 'DocN', 'Doc5', 'Doc10')
  rel <- c(1,0,1,0)
  actual <- data.frame(query=queries, Q0= rep(0,4), 
                       docIDs=docs, rel=rel, stringsAsFactors=F)
  
  runs <- read.runs(c(test_runFile1,test_runFile2),runids = c('A', 'B'))
  subset_qrels <- qrels.createSubset(runs, test_qrelsFile, 3, 'topk')
  expect_equal(data.frame(subset_qrels, stringsAsFactors=F), actual)
})




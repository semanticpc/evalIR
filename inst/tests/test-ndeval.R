context("Tests using ndeval tools for diversity")

test_qrelsFile <- system.file('data/tests/toy.div.qrels', 
                              package='evalIR',mustWork=T)

test_runFile1 <- system.file('data/tests/toy.div.res',
                             package='evalIR', mustWork=T)

test_that("eval.diversity: Simple*)",{
  test_q <- read.qrels(test_qrelsFile, type='diversity')
  test_r <- read.runs(runPaths=c(test_runFile1),limit=1000)
})
context("Testing Diversity Measures using the ndeval tool")

test_runsFolder <- system.file('data/tests/test_runs', 
                               package='evalIR',mustWork=T)
test_expectedFolder <- system.file('data/tests/test_expected', 
                                   package='evalIR',mustWork=T)
test_qrelsFile <- system.file('data/tests/trec09.div.qrels',
                              package='evalIR',mustWork=T)

test_ndeval <- function(rid, res, runPaths){
  expected <- read.table(paste0(test_expectedFolder, '/', 
                                basename(runPaths[rid]),'.ndeval'), 
                         header=T,sep=',')
  
  # Testing S-Recall
  expect_equal(cor(round(expected$strec.5[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'srecall@5'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$strec.10[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'srecall@10'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$strec.20[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'srecall@20'),
               1,
               tol=0.001)
  
  # Testing ERR-IA
  expect_equal(cor(round(expected$ERR.IA.5[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'ERRIA@5'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$ERR.IA.10[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'ERRIA@10'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$ERR.IA.20[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'ERRIA@20'),
               1,
               tol=0.001)
  
  # Testing Alpha nDCG
  expect_equal(cor(round(expected$alpha.nDCG.5[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphanDCG@5'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$alpha.nDCG.10[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphanDCG@10'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$alpha.nDCG.20[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphanDCG@20'),
               1,
               tol=0.001)
  
  # Testing Alpha DCG
  expect_equal(cor(round(expected$alpha.DCG.5[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphaDCG@5'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$alpha.DCG.10[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphaDCG@10'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$alpha.DCG.20[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'AlphaDCG@20'),
               1,
               tol=0.001)
  
  # Testing NRBP and nNRBP
  expect_equal(cor(round(expected$NRBP[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'NRBP'),
               1,
               tol=0.001)
  
  expect_equal(cor(round(expected$nNRBP[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'nNRBP'),
               1,
               tol=0.001)
  
  
  # Testing MAPIA
  expect_equal(cor(round(expected$MAP.IA[1:50],4), 
                   subset(res, runid == basename(runPaths[rid]))$'MAPIA'),
               1,
               tol=0.001)
}

test_that("eval.diversity: Simple*)",{
  
  test_runFiles <- paste0(test_runsFolder,'/',list.files(test_runsFolder))
  runPaths  <- sample(test_runFiles, 5)
  
  test_runs <- read.runs(runPaths, basename(runPaths), limit=1000)
  test_qrels <- read.qrels(test_qrelsFile, type='diversity')
  
  res <- eval.diversity(test_qrels, test_runs, ranks=c(5,10,20),sort='ndevalSort')
  
  test_ndeval(1, res, runPaths)
  test_ndeval(2, res, runPaths)
  test_ndeval(3, res, runPaths)
  test_ndeval(4, res, runPaths)
  test_ndeval(5, res, runPaths)
  
  
})


#runPaths <- c("/Library/Frameworks/R.framework/Versions/3.0/Resources/library/evalIR/data/tests/test_runs/udelSimPrune", "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/evalIR/data/tests/test_runs/Sab9wtBDiv1", "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/evalIR/data/tests/test_runs/THUIR09FuClu", "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/evalIR/data/tests/test_runs/MSRABASE", "/Library/Frameworks/R.framework/Versions/3.0/Resources/library/evalIR/data/tests/test_runs/uogTrDYCcsB" )
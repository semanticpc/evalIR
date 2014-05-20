context("Diveristy Evaluation Measures")

test_qrels_grades <- matrix(c(c(a=0, b=0, c=0, d=0, e=1, f=1, g=0, h=1, i=0, j=0),
                        c(a=1, b=1, c=1, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=0, f=0, g=1, h=0, i=0, j=0),
                        c(a=1, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=1, f=0, g=0, h=0, i=0, j=0)),
                        ncol=6,byrow=F)
colnames(test_qrels_grades) <- 1:6
rownames(test_qrels_grades) <- LETTERS[1:10]
                   
test_grades<- test_qrels_grades

rankCutoff <- 5
ranks <- c(5,10, 20)


test_that("simple diversity measure (num_*)",{
  
  expect_equal(num_ret(test_grades), 10)
  expect_equal(num_ret(test_grades, rankLimit=20), 10)
  expect_equal(num_ret(test_grades, rankLimit=5), 5)
    
})



test_that("Alpha-DCG",{
  res <- AlphaDCG(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.423341, tol=0.001)
  expect_equal(res[2], 0.494401,tol=0.001)
  expect_equal(res[3], 0.494231,tol=0.001)
  
})


test_that("Alpha-nDCG",{
  
  res <- AlphanDCG(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.770669, tol=0.001)
  expect_equal(res[2], 0.875999,tol=0.001)
  expect_equal(res[3], 0.875999,tol=0.001)
  
  
})

test_that("Subtopic Recall",{
  
  res <- srecall(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.800000, tol=0.001)
  expect_equal(res[2], 1.000000,tol=0.001)
  expect_equal(res[3], 1.000000,tol=0.001)
  
})


test_that("ERR-IA",{
  
  res <- ERRIA(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.396974, tol=0.001)
  expect_equal(res[2], 0.431529,tol=0.001)
  expect_equal(res[3], 0.431477,tol=0.001)
  
  
})


test_that("MAP-IA",{
  
  res <- MAPIA(test_qrels_grades, test_grades)
  expect_equal(res[1], 0.529127, tol=0.001)
  
})


test_that("Simple Precision",{
  
  res <- simple_prec(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.8, tol=0.001)
  expect_equal(res[2], 0.7,tol=0.001)
  expect_equal(res[3], 0.35,tol=0.001)
  
})

test_that("Precision-IA",{
  
  res <- PrecIA(test_qrels_grades, test_grades, ranks)
  expect_equal(res[1], 0.24000, tol=0.001)
  expect_equal(res[2], 0.18000,tol=0.001)
  expect_equal(res[3], 0.09000,tol=0.001)
  
})


test_that("DCG-IA",{
  
#   res <- DCGIA(test_qrels_grades, test_grades, ranks)
#   expect_equal(res[1], 0.24000, tol=0.001)
#   expect_equal(res[2], 0.18000,tol=0.001)
#   expect_equal(res[3], 0.09000,tol=0.001)
  
})



test_that("Novelty Rank Biased Precision",{
  
  res <- NRBP(test_qrels_grades, test_grades)
  expect_equal(res[1], 0.370605, tol=0.001)
  
})

test_that("normalized Novelty Rank Biased Precision",{
  
  res <- nNRBP(test_qrels_grades, test_grades)
  expect_equal(res[1], 0.736321, tol=0.001)
  
})


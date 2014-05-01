context("Diveristy Evaluation Measures")

# library(evalIR)
# library(testthat)
# test_file('tests//testthat//test-diversity.R')

qrels_grades <- matrix(c(c(a=0, b=0, c=0, d=0, e=1, f=1, g=0, h=1, i=0, j=0),
                        c(a=1, b=1, c=1, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=0, f=0, g=1, h=0, i=0, j=0),
                        c(a=1, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
                        c(a=0, b=0, c=0, d=0, e=1, f=0, g=0, h=0, i=0, j=0)),
                        ncol=6,byrow=F)
colnames(qrels_grades) <- 1:6
rownames(qrels_grades) <- LETTERS[1:10]
                   
grades <- qrels_grades

rankCutoff <- 5
ranks <- c(5,10, 20)


test_that("simple diversity measure (num_*)",{
  
  expect_equal(num_ret(grades), 10)
  expect_equal(num_ret(grades, rankLimit=20), 10)
  expect_equal(num_ret(grades, rankLimit=5), 5)
    
})



test_that("Alpha-DCG",{
  res <- AlphaDCG(qrels_grades, grades, ranks)
  expect_equal(res[1], 0.423341, tol=0.001)
  expect_equal(res[2], 0.494401,tol=0.001)
  expect_equal(res[3], 0.494231,tol=0.001)
  
})


test_that("Alpha-nDCG",{
  
  res <- AlphanDCG(qrels_grades, grades, ranks)
  expect_equal(res[1], 0.770669, tol=0.001)
  expect_equal(res[2], 0.875999,tol=0.001)
  expect_equal(res[3], 0.875999,tol=0.001)
  
  
})

test_that("Subtopic Recall",{
  
  res <- srecall(qrels_grades, grades, ranks)
  expect_equal(res[1], 0.800000, tol=0.001)
  expect_equal(res[2], 1.000000,tol=0.001)
  expect_equal(res[3], 1.000000,tol=0.001)
  
})


test_that("ERR-IA",{
  
  res <- ERRIA(qrels_grades, grades, ranks)
  expect_equal(res[1], 0.396974, tol=0.001)
  expect_equal(res[2], 0.431529,tol=0.001)
  expect_equal(res[3], 0.431477,tol=0.001)
  
  
})


test_that("MAP-IA",{
  
  res <- MAPIA(qrels_grades, grades)
  expect_equal(res[1], 0.529127, tol=0.001)
  
})


test_that("Precision-IA",{
  
  res <- PrecIA(qrels_grades, grades, ranks)
  expect_equal(res[1], 0.24000, tol=0.001)
  expect_equal(res[2], 0.18000,tol=0.001)
  expect_equal(res[3], 0.09000,tol=0.001)
  
})


test_that("DCG-IA",{
  
#   res <- DCGIA(qrels_grades, grades, ranks)
#   expect_equal(res[1], 0.24000, tol=0.001)
#   expect_equal(res[2], 0.18000,tol=0.001)
#   expect_equal(res[3], 0.09000,tol=0.001)
  
})



test_that("Novelty Rank Biased Precision",{
  
  res <- NRBP(qrels_grades, grades)
  expect_equal(res[1], 0.370605, tol=0.001)
  
})

test_that("normalized Novelty Rank Biased Precision",{
  
  res <- nNRBP(qrels_grades, grades)
  expect_equal(res[1], 0.736321, tol=0.001)
  
})


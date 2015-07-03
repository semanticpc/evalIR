context("Ad-hoc Evaluation Measures")

test_qrels_grades <- c(Doc1=0, Doc2=1, Doc3=0, Doc4=1, Doc5=1,
                   Doc6=0, Doc7=1, Doc8=1, Doc9=1, Doc10=0)
test_grades <- c(Doc5=1, Doc10=0, DocXa=0, Doc9=1, Doc6=0,
             Doc1=0, Doc8=1, Doc4=1, DocX=0, Doc7=1)
rankCutoff <- 5
ranks <- c(3,5,10)



test_that("simple ad-hoc measure (num_*)",{
  
  expect_equal(num_ret(test_grades), 10)
  expect_equal(num_ret(test_grades, rankLimit=20), 10)
  expect_equal(num_ret(test_grades, rankLimit=5), 5)
  
  expect_equal(num_rel_ret(test_grades), 5)
  expect_equal(num_rel_ret(test_grades, rankLimit=20), 5)
  expect_equal(num_rel_ret(test_grades, rankLimit=5), 2)
  
  expect_equal(num_rel(test_qrels_grades), 6)  
})

test_that("Reciprocal Rank",{
  
  expect_equivalent(recip_rank(test_grades), 1)
  
})



# Precision Based Metrics


test_that("Precision",{
  
  expect_equal(Prec(test_qrels_grades,test_grades, ranks)[1], 0.3333, tol=0.001)
  expect_equal(Prec(test_qrels_grades,test_grades, ranks)[2], 0.4, tol=0.001)
  expect_equal(Prec(test_qrels_grades,test_grades, ranks)[3], 0.5, tol=0.001)
  
})

test_that("Graded Precison",{
  
  #expect_equal(GradedPrec(test_qrels_grades,test_grades, ranks), length(test_grades))
  
})

test_that("nDCG",{
  res <- nDCG(test_qrels_grades,test_grades, c(3))
  expect_equal(res, 0.46928, tol=0.0001)
  res <- nDCG(test_qrels_grades,test_grades, c(5))
  expect_equal(res, 0.48523, tol=0.0001)
  res <- nDCG(test_qrels_grades,test_grades, c(10))
  expect_equal(res, 0.71673, tol=0.0001)
  
})


test_that("DCG",{
  
  #expect_equal(DCG(test_qrels_grades,test_grades, ranks), length(test_grades))
  
})

test_that("ERR",{
  res <- ERR(test_qrels_grades,test_grades, c(3))
  expect_equal(res, 0.06250, tol=0.0001)
  res <- ERR(test_qrels_grades,test_grades, c(5))
  expect_equal(res, 0.07715, tol=0.0001)
  res <- ERR(test_qrels_grades,test_grades, c(10))
  expect_equal(res, 0.09626, tol=0.0001)
  
})

test_that("nERR",{
  res <- nERR(test_qrels_grades,test_grades, c(3))
  expect_equal(res, 0.56762, tol=0.0001)
  res <- nERR(test_qrels_grades,test_grades, c(5))
  expect_equal(res, 0.58164, tol=0.0001)
  res <- nERR(test_qrels_grades,test_grades, c(10))
  expect_equal(res, 0.68668, tol=0.0001)
  
  
})





# Recall Based Metrics

test_that("Rprec",{
  
  #expect_equal(Rprec(test_qrels_grades,test_grades, rankCutoff), length(test_grades))
  
})

test_that("Recall",{
  
  expect_equal(recall(test_qrels_grades, test_grades, rankLimit=5), 0.3333, tol=0.001)
  expect_equal(recall(test_qrels_grades, test_grades, rankLimit=10), 0.8333, tol=0.001)
  expect_equal(recall(test_qrels_grades, test_grades, rankLimit=1000), 0.8333, tol=0.001)
  
})


test_that("Average Precision",{
  expect_equal(AP(test_qrels_grades, test_grades), 0.4881, tol=0.0001)
  
  
})

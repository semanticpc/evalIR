context('Math, TREC, and all other utility tests')



test_that("Math Utils: Harmonic Number", {
  expect_equal(harmonic_no(2), c(1,1.5))
  expect_equal(harmonic_no(5), c(1,1.5,1.833,2.083,2.283), tolerance=0.001)
})



test_that("TREC Utils: Parse Web Track Topics", {
  
})


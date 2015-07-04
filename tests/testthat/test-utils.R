context('Math, TREC, and all other utility tests')

test_that("Math Utils: Harmonic Number", {
  expect_equal(harmonic_no(2), c(1,1.5))
  expect_equal(harmonic_no(5), c(1,1.5,1.833,2.083,2.283), tolerance=0.001)
})


test_that("parse trec web fulltopics XML file", {
  testfile_fulltopics <- system.file('extdata/tests/trec09.web.fulltopics',
                          package='evalIRamt', mustWork=T)

  parsedFile <- parseWebTopicDesc(testfile_fulltopics)
  desc_1 <-  "Find information on President Barack Obamas family history including genealogy national origins places and dates of birth etc."

  expect_equal(parsedFile[[1]]$type, "faceted")
  expect_equal(parsedFile[[1]]$text, "obama family tree")
  expect_equal(parsedFile[[1]]$desc, desc_1)
  for(i in seq(1, 50)) expect_equal(parsedFile[[i]]$number, i)
})

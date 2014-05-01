context('Meta Search Algorithms')


rank_matrix <- matrix(rep(1:4,3), ncol=3, 
                      dimnames=list(letters[1:4], c("sys1","sys2","sys3")))

# test_that("metaAP algorithm", {
#   docs <- c(a=1,b=1.5,c=1.833,d=2.083)
#   expect_equal(metaAP(rank_matrix, "mean"), equals(docs, tolerance=0.001))
# })
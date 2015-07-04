# context("Triplet Diveristy Evaluation Measures")
# 
# test_qrels_grades <- matrix(c(c(a=0, b=0, c=0, d=0, e=1, f=1, g=0, h=1, i=0, j=0),
#                               c(a=1, b=1, c=1, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
#                               c(a=0, b=0, c=0, d=0, e=0, f=0, g=1, h=0, i=0, j=0),
#                               c(a=1, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
#                               c(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0),
#                               c(a=0, b=0, c=0, d=0, e=1, f=0, g=0, h=0, i=0, j=0)),
#                             ncol=6,byrow=F)
# colnames(test_qrels_grades) <- 1:6
# rownames(test_qrels_grades) <- LETTERS[1:10]
# 
# test_grades<- test_qrels_grades
# 
# rankCutoff <- 5
# ranks <- c(5,10, 20)
# 
# 
# test_that("getTripletUtility",{
#   test_qrelsFile <- system.file('data/tests/toy.triplet.qrels', 
#                                 package='evalIR',mustWork=T)
#   
#   qrels <- read.qrels(test_qrelsFile, type='triplets')
#   
#   utils <- getTripletUtility(qrels, 1)
#   #print(utility.triplet(utils, c('a'), c('b')))
#   #print(utility.triplet(utils, c(''), c('b')))
#   #print(utility.triplet(utils, c('a','g'), c('e')))
#   print(DivTripletMeasure(utils, c('a','b','e'), ranks=c(3),
#                           F_type='average',P_k='RBP'))
#   
#   
# })
# 
# 

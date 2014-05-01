library(evalIR)

prf <- read.table('~/work/data/experiments//prfSim//userPreferences.txt', 
                  header=T, sep=',')
prf$doc1 <- as.character(prf$doc1)
prf$doc2 <- as.character(prf$doc2)
queries <- sort(unique(prf$Qid))

qrels <- read.qrels(qrelsPath='/Users/praveen/work//data//qrels//trec13.div.qrels', type='diversity')

getSubtopicOverlap <- function(qrels, doc1, doc2, qid){
  sum(qrels$getMatrix(qid)[doc1,] + qrels$getMatrix(qid)[doc2,] > 1)
}

prfOverlap <- c()
for(q in queries){
  prf_sub <- subset(prf, Qid == q)
  overlap <- apply(prf_sub, 1, function(x) getSubtopicOverlap(qrels, 
                                                              x[3], x[4], q))
  prf_sub <- cbind(prf_sub, ST_Overlap=overlap)
  prfOverlap <- rbind(prfOverlap, prf_sub)
  
}



library(evalIR, quietly=T)
library(plyr, quietly=T)

compute_metaAP <- function(x, runs, pooling_depth){
  if(x == 0) return(data.frame())
  else{
    scores <- metaSearch.metaAP(runs$getRankMatrix(x), "mean" ,1000)
    return(data.frame(docID=names(scores), metaAP_socres=scores))  
  }
}


args <- commandArgs(trailingOnly = T)
runFolder <- args[1]

runFiles <- list.files(runFolder, full.names=T)

runIDs <- basename(runFiles)
runs <- read.runs(runPaths= runFiles, runids= runIDs, limit= 1000)
metaAP <- adply(runs$getQueries(), 1, compute_metaAP, runs, 1000)
colnames(metaAP)[1] <- 'query'
metaAP$query <- as.numeric(metaAP$query) + 200

write.table(metaAP, file="metaAP.all", quote=F, row.names=F)
#pooled_docs <- adply(runs$getQueries(), 1, function(x) 
#  data.frame(docID=pooling.topk(runs$getRankMatrix(x) ,1000)))
#colnames(pooled_docs)[1] <- 'query'
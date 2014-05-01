library(evalIR)
source("paths.R")

process <- function(query, mat, folder, qrels){
  nmat <- c()
  apply(mat, 2, FUN = function(x) {x[x==0] <- NA})
  run_min <- apply(mat, 2, FUN = function(x) {min(x,na.rm=T)})
  for(i in 1:ncol(mat)){ 
    tmp <- mat[,i]
    tmp[tmp==0] <- run_min[i]
    nmat<- cbind(nmat, tmp)
  }
  colnames(nmat) <- colnames(mat)
  docnames <- paste0('# ',row.names(nmat))
  judgments <- qrels$judgeQuery(query, row.names(nmat))
  
  nmat <- matrix(paste0(1:ncol(nmat), ':', t(nmat)), byrow=T, nrow=nrow(nmat))
  nmat <- cbind(rel=judgments, query=paste0('qid:',rep(query, nrow(nmat))),
                nmat, docid=docnames)
  
  write.table(nmat, file=paste0(folder, '/', query, '.scores'), 
              append=F, sep=' ', quote=F, row.names=F, col.names=F)
  
}

TRECRun2Features <- function(runsPaths, qrelsPath){
  runFiles <- runsPaths
  runIDs <- basename(runFiles)
  runs <- read.runs(runPaths= runFiles, runids= runIDs, limit= 5000)
  qrels <- read.qrels(qrelsPath, type="adhoc")
  
  for(query in qrels$getQueries()){
    process(query, runs$getScoreMatrix(query), 
            paste0(data_home,'/features/'), qrels)
  }
    
  return(runs)
  
}





res <- TRECRun2Features(runsPaths, qrelsPath)
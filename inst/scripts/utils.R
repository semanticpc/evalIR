getDataPaths <- function(base_data_dir, Rdata_path){
  rm(list=ls(pattern="path."))
  data_dirs <- list.dirs(base_data_dir,recursive=T)
  tmp <- gsub(paste(base_data_dir,'/',sep=''),"",data_dirs[2:length(data_dirs)])
  tmp <- gsub('/','.',tmp)
  for(i in 1:length(tmp)) { 
    assign(paste('path',tmp[i],sep='.'), data_dirs[i+1], envir = .GlobalEnv)
  }  
  save(file=Rdata_path, list=ls(pattern="path."))
}

#qrelsFile <- system.file('sampledata/test_files/toy.adhoc.qrels',package='evalIR')
#resFile <- system.file('sampledata/test_files/toy.adhoc.res',package='evalIR')

trec_eval2Table <- function(qrelsFile, runFile){
  try(system(paste("trec_eval -q -mall_trec > /tmp/trec_eval.res",
                   qrelsFile, resFile), intern = TRUE))
  res <- scan("/tmp/trec_eval.res", what=list(measure='',qid='',score=''),
              quiet=T)
  
  measureNames <- res$measure[1:91]
  
  queries <- unique(res$qid)
  queries <- queries[queries != 'all']
  
  scores <- matrix(res$score[1:(length(queries)*91)],nrow=3,byrow=T)
  
  res_table <- data.frame(cbind(queries, scores))
  colnames(res_table) <- c('qid', measureNames)
  
  
  file.remove('/tmp/trec_eval.res')
  
  return(res_table)
}
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

library(RCurl)
library(getopt)
library(evalIR)

pool_documents <- function(depth){
  trec09 <- c('/usa/ravichan/work/diversity/runs/trec/trec2009')
  trec10 <- c('/usa/ravichan/work/diversity/runs/trec/trec2010')
  trec11 <- c('/usa/ravichan/work/diversity/runs/trec/trec2011')
  
  run_files_path <- c()
  run_files_runids <- c()
  
  trec09 <- list.files(path=trec09, full.names=T)
  trec09_runids <- paste("trec09_", basename(trec09), sep='')
  
  trec10 <- list.files(path=trec10, full.names=T)
  trec10_runids <- paste("trec10_", basename(trec10), sep='')
  
  trec11 <- list.files(path=trec11, full.names=T)
  trec11_runids <- paste("trec11_", basename(trec11), sep='')
  
  run_files_path <- c(trec11)#, trec10, trec11)
  run_files_runids <- c(trec11_runids)#, trec10_runids, trec11_runids)
  
  
  runs <- read.runs(runPaths=run_files_path,
                    runids=run_files_runids, limit=depth)
  
  res <- data.frame(query= numeric(0), docID = character(0))
  
  for(q in sort(runs$getQueries(), decreasing=T)){
    docs <- pooling.topk(runs$getRankMatrix(q), depth)  
    df_documents <- data.frame(query=rep(q,length(docs)),docID=names(docs))
    
    
    res <- rbind(data.frame(query=rep(q,length(docs)),
                            docID=names(docs)), res)
    
    
  }
  
  return(res) 
}


fetchClueweb09_docs <- function(docs_list, output_folder){
  h = getCurlHandle(header = TRUE, userpwd = "udel-irl:udblue&gold", 
                    netrc = TRUE)
  base <- 'http://boston.lti.cs.cmu.edu:8085/clueweb09/render/renderpage.cgi?id='
  for(url in docs_list){
    if(file.exists(paste(output_folder, '/', url,'.html', sep=''))) next
    write(getURL(paste(base,'',url,sep=''),  
                 verbose = F,
                 userpwd = "udel-irl:udblue&gold"), 
          file=paste(output_folder, '/', url,'.html', sep=''))
    
  }
}

output_folder <- '/usa/ravichan/public_html/clueweb09'

documents_df <- pool_documents(5)
documents <- unique(as.character(documents_df$docID))

fetchClueweb09_docs(documents, output_folder)
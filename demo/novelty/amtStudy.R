library(evalIR)
library(RMySQL)
library(RCurl)

pool_documents <- function(depth){
  trec09 <- c('demo/data/diversity/trec2009')
  trec10 <- c('demo/data/diversity/trec2010')
  trec11 <- c('demo/data/diversity/trec2011')
  
  run_files_path <- c()
  run_files_runids <- c()
  
  trec09 <- list.files(path=trec09, full.names=T)
  trec09_runids <- paste("trec09_", basename(trec09), sep='')
  
  trec10 <- list.files(path=trec10, full.names=T)
  trec10_runids <- paste("trec10_", basename(trec10), sep='')
  
  trec11 <- list.files(path=trec11, full.names=T)
  trec11_runids <- paste("trec11_", basename(trec11), sep='')
  
  run_files_path <- c(trec09, trec10, trec11)
  run_files_runids <- c(trec09_runids, trec10_runids, trec11_runids)
  
  
  runs <- read.runs(runPaths=run_files_path,
                    runids=run_files_runids, limit=depth)
  
  res <- data.frame(query= numeric(0), docID = character(0), 
                    status = logical(0))
  for(q in sort(runs$getQueries(), decreasing=T)){
    docs <- pooling.topk(runs$getRankMatrix(q), depth)  
    df_documents <- data.frame(query=rep(q,length(docs)),docID=names(docs))
    df_urls <- getURLs(names(docs))
    
    if ( nrow(df_urls) > 0 ){
      doc_urls <- merge(df_documents, df_urls, by=c("docID"), all.x=T)
      status <- sapply(doc_urls$URL,  url.exists)
      doc_urls <- cbind(doc_urls, status)
      res <- rbind(doc_urls, res)  
    } else{
      res <- rbind(data.frame(query=rep(q,length(docs)),
                              docID=names(docs), 
                              URL=rep('NA', length(docs)),
                              status=rep(F, length(docs))) , res)
    }
    break
  }
  return(res)
}

getURLs <- function(docids){
  con <- dbConnect(MySQL(),
                   user="praveen", password="",
                   dbname="test", host="127.0.0.1")

  rs <- dbSendQuery(con, 
                    paste("SELECT * FROM clueweb_urls WHERE id in (", 
                          paste( paste('\'',docids,'\'',sep=''), collapse=','), 
                          ");"))
  results <- fetch(rs)
  if(nrow(results) > 0) names(results) <- c('docID', 'URL')
  dbDisconnect(con)
  return(results)
}


# First get the pooled documents
res <- pool_documents(5)

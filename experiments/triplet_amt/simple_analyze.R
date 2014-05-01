tableToPickTopics <- function(){
  require(evalIR)
  require(plyr)
  source('paths.R')
  trec12 <- read.qrels(qrelsPath='~/work/data/qrels/trec12.div.qrels', 
                       type='diversity')
  trec12RelCounts <- sapply(trec12$getQueries(), 
                            function(x) nrow(trec12$getMatrix(x)))
  
  trec12CoverageScore <- sapply(trec12$getQueries(), 
                                function(x) 
                                  mean(apply(trec12$getMatrix(x), 1, sum) / 
                                         ncol( trec12$getMatrix(x))))
  
  topics <- read.table('~/work/data/topics/trec12.web.queries',sep=':')
  fulltopics <- parseWebTopicDesc('~/work/data/topics/trec12.web.fulltopics')
  
  qtype <- sapply(fulltopics, function(x) x$type)
  
  getPooledDocsStats <- function(query){
    pooled_docs <- data.frame(docID=pooling.topk(runs$getRankMatrix(query$qid), 
                                                 pooling_depth))
    
    data.frame(NumOfDocs=nrow(pooled_docs))
  }
  
  runs_path <- '~/work/data/runs/diversity/trec2012'
  runFiles <- list.files(path=runs_path, full.names=T)
  runIDs <- basename(runFiles)
  pooling_depth <- 5
  
  runs <- read.runs(runPaths= runFiles, runids= runIDs, limit= pooling_depth)
  
  
  numOfPooledDocs <- ddply(data.frame(qid=trec12$getQueries()), 
                     .(qid), 
                     getPooledDocsStats)

  numOfTriplets <- sapply(numOfPooledDocs$NumOfDocs, 
                          function(x) x * ncol(combn(x-1,2)))
  
  res <- data.frame(qid=trec12$getQueries(), 
                    topics=topics$V2,
                    NumRelDocs=trec12RelCounts, 
                    Coverage=trec12CoverageScore,
                    type=qtype,
                    numOfPooledDocs=numOfPooledDocs$NumOfDocs,
                    numOfTriplets=numOfTriplets)
  res
}










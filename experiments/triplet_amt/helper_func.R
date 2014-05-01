sample.triplets <- function(documents, size, prob=F){
  documents$docID <- as.character(documents$docID)
  
  size <- size + (size %% 4)
  sampled_triplets <- data.frame( left_doc = character(size),
                                  right_doc = character(size), 
                                  top_doc = character(size), 
                                  stringsAsFactors=F)
  
  sampled_set <- character(size*2)
  for(i in 1:size){
    if(prob == F) row <- sample(nrow(documents), 3, replace=F)
    else row <- sample(nrow(documents), 3, replace=F, prob=documents$prob)
    code <- paste("d",row, sep='',collapse='')
    
    
    while (is.element(code,sampled_set)) {
      if(prob == F) row <- sample(nrow(documents), 3, replace=F)
      else row <- sample(nrow(documents), 3, replace=F, prob=documents$prob)
      code <- paste("d",row, sep='',collapse='')
    }
    
    sampled_set[i*2] <- code
    tmp <- row[3] 
    row[3] <- row[2]
    row[2] <- tmp
    code <- paste("d",row, sep='',collapse='')
    sampled_set[(i*2) - 1] <- code
    triplet_row  <- data.frame( top_doc = documents[row[1],"docID"],
                                left_doc = documents[row[2],"docID"],
                                right_doc = documents[row[3],"docID"], 
                                stringsAsFactors=F)
    sampled_triplets[i,] <- triplet_row
    
  }
  return(sampled_triplets)
}

oldprepare.HIT <- function(triplets, topicDesc, sample_size){
  baseURL <- 'http://ir.cis.udel.edu/~ravichan/clueweb09/'
  triplets$top_doc <- paste(baseURL, triplets$top_doc, '.html', sep='')
  triplets$left_doc <- paste(baseURL, triplets$left_doc, '.html', sep='')  
  triplets$right_doc <- paste(baseURL, triplets$right_doc, '.html', sep='')
  
  query <- unique(triplets$query)
  qText <- topicDesc[[as.character(query)]]$text
  qDesc <- topicDesc[[as.character(query)]]$desc
  
  sampled_triplet <- c()
  HITs <- c()
  
  no_of_HITS <- ceiling(sample_size / 4)
  internal_HIT_id <- 1
  
  for(hit_count in 1:no_of_HITS){
    available_triplets <- setdiff(1:nrow(triplets), sampled_triplet)
    
    # Sample for the current HIT 
    current_sample <- sample(nrow(triplets), 4)
    trap <- sample(setdiff(1:nrow(triplets), current_sample), 1) 
    
    # Triplets ones sampled do not get picked again 
    sampled_triplet <- c(sampled_triplet, current_sample)
    
    # We use the sample function here to randonmize the position of the trap
    #  HITS keeps track of all the AMT_HIT created
    current_HIT <- cbind(internalID=rep(internal_HIT_id, 5), 
                         qText=rep(qText, 5),
                         qDesc=rep(qDesc, 5),
                         sample(triplets[c(current_sample, trap),]))
    
    
    
    HITs <- rbind(HITs, current_HIT)
    
    internal_HIT_id = internal_HIT_id + 1
  }
  return(HITs)
}

analyze <- function(triplets, runs){
  q <- unique(triplets$query)
  mat <- runs$getRankMatrix(q)
  metaAP <- metaAP(mat, "mean")
  
  range.check <- function(ls) {
    sum(sapply(ls, FUN=function(x) x %in% c(1,2,3,4,5)))
  }
  
  #t <- ddply(triplets, 1, function(x) range.check(mat[x$top_doc,]))$V1
  #l <- ddply(triplets, 1, function(x) range.check(mat[x$left_doc,]))$V1
  #r <- ddply(triplets, 1, function(x) range.check(mat[x$right_doc,]))$V1
  
  docs.freq <- sort(table(unlist(c(subset(triplets, query == q, 
                                          select=c("left_doc", 
                                                   "right_doc", 
                                                   "top_doc"))))), decreasing=T)
  
  c <- aaply(names(docs.freq), 1, function(x) range.check(mat[x,]))
  
  res <- data.frame(row.names=names(docs.freq), 
                    freq=docs.freq, 
                    metaAP=metaAP[names(docs.freq)],
                    counts=c)
  return(data.frame(kendall=cor(res$freq, res$metaAP, method="kendall"),
                    counts=sum(res$counts)))
  #return(data.frame(top=sum(t), left= sum(l), right= sum(r)))
  
}



create.AMTHITs <- function(amtHandle, hittype, HIT_set, batch_name){
  
  
  

}
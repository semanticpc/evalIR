
# eval.diversity_triplets(){
#  
# }


DivTripletMeasure <- function(qrel_utilities, run, ranks, 
                              F_type=c('average','min'),
                              P_k=c('RBP', 'DCG', 'RR')){
  
  maxRank <- max(ranks)
  run <- run[1:maxRank]
  # Compute document utility U(d_i|S)
  if(F_type == 'average'){
    first_doc <-  utility.triplet(qrel_utilities, c(''), run[1], 'ave')
    print(first_doc)
    run.utility <- sapply(2:length(run), function(x) 
      utility.triplet(qrel_utilities, c('',run[1:(x-1)]), run[x], 'ave'))
    print(run.utility)
    run.utility <- c(first_doc, run.utility)
  }
  else if(F_type == 'min'){
    first_doc <-  utility.triplet(qrel_utilities, c(''), run[1], 'min')
    run.utility <- sapply(2:length(run), function(x) 
      utility.triplet(qrel_utilities, c('',run[1:(x-1)]), run[x], 'min'))
    run.utility <- c(first_doc, run.utility)
  }
  print(run.utility)
  run.utility <- cumsum(run.utility)
  print(run.utility)
  # Compute Discount or P_k() 
  if(P_k == 'RBP')
    run.discount <- P.RBP (maxRank)
  else if(P_k == 'DCG')
    run.discount <- P.DCG (maxRank)
  else if(P_k == 'RR')
    run.discount <- P.RR (maxRank)
  print(run.discount)
  run.prfscore <- cumsum(run.discount * run.utility)
  
  names(run.prfscore) <- c()
  
  return(run.prfscore[ranks])
  
}


# Some helper functions

getTripletUtility <- function(qrel_ptr, query, docs=NULL){

  if(is.null(docs)){
    docs <- qrel_ptr$getDocuments(query)
    conditions <- qrel_ptr$getConditions(query)
  }else{
    conditions <- c('', docs)
  }
  
  computeScore <- function(cond_doc){
    cond_doc <- as.character(cond_doc$conditions)
    conds <- rep(cond_doc, length(docs))
    appearance <- qrel_ptr$getCondApperanceCount(1, conds, docs) 
    prefCount <- qrel_ptr$getCondPrefCount(1, conds, docs)
      return(data.frame(doc=docs,utility=prefCount/(appearance+2)))
  }
  
  util_score <- ddply(data.frame(conditions=conditions), .(conditions), 
                      computeScore)

  return(util_score)
}
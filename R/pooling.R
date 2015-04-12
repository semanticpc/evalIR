#' @title Sum of Vector Elements
#'
#' @description
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the \code{Summary} group generic.  For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.'

pooling.topk <- function(rank_matrix, k){
  
  min_ranks <- apply(rank_matrix, 1, function(x) {min(x[x > 0])})
  return(names(min_ranks[min_ranks <= k]))
}

qrels.createSubset <- function(runs, qrels_path, k, 
                               type=c('topk','moveToFront')){
  # Read the query file 
  qrels <- read.table(qrels_path, header = F)
  names(qrels) <- c('query', 'Q0', 'docIDs', 'rel')
  queries <- unique(qrels$query)
  qrels$docIDs <- as.character(qrels$docIDs)

  if(type == 'topk')
    pooledDocs <- adply(queries, 1, function(x) 
      data.frame(query=x,docIDs=pooling.topk(runs$getRankMatrix(x) , k)))
  pooledDocs$X1 <- NULL
  pooledDocs$docIDs <- as.character(pooledDocs$docIDs)
  
  new_qrels <- adply(queries, 1, function(x) 
    subset(qrels, (docIDs %in% subset(pooledDocs, query == x)$docIDs) 
                      &  query == x))
  new_qrels$X1 <- NULL
  
  return(new_qrels)
  
}
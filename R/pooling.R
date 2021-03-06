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
  rank_matrix <- apply(rank_matrix, 1:2, function(x) if(x == 0) NA else x)
  min_ranks <- apply(rank_matrix, 1, min, na.rm=T)
  return(min_ranks[min_ranks <= k])
}
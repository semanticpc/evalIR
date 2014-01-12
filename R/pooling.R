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
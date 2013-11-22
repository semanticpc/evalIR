#' @title Read Judgments 
#'
#' @description
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' @details
#' This function: methods can be defined for it directly
#' or via the \code{Summary} group generic.  For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.'
read.qrels <- function(qrelsPath, type=c("adhoc","diversity")){
  type <- match.arg(type)
  if(type == "adhoc") return( new(AdhocQrels, qrelsPath))
  else if(type == "diversity") return( new(DivQrels, qrelsPath))
}


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
read.runs <- function(runPaths, runids=NULL, limit=1000,
                      sort=c("trecSort", "rankSort")){
  sort <- match.arg(sort)
  
  if(is.null(runids)) runids <- sapply(runPaths, basename)
  
  if(length(unique(runids)) != length(runPaths)) warning("Duplicate RunIDs found")
    
  new(Runs, runPaths, runids, limit)
}
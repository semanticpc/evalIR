#' @title Read Judgments 
#'
#' @description
#' \code{read.qrels} returns a representation of the qrels.
#' 
#' @param qrelsPath path to the qrels file
#' 
#' @param type type  of the qrels file. ('adhoc', diversity') are currently 
#' supported.
#' 
#' @details
#' The function creates a C++ object containing various information parsed from
#' the qrels file. 
#' 
#' @export
read.qrels <- function(qrelsPath, type=c("adhoc","diversity")){
  type <- match.arg(type)
  suppressPackageStartupMessages(require(tools, quietly=T))
  qrelsPath <- file_path_as_absolute(qrelsPath)
  if(type == "adhoc") return( new(AdhocQrels, qrelsPath))
  else if(type == "diversity") return( new(DivQrels, qrelsPath))
}


#' @title Read System Runs
#'
#' @description
#' \code{read.runs} returns a representation of a set of runs.
#' 
#' @param runPaths a character vector containing a set of the paths of the runs.
#' 
#' @param runids a character vector containing a set of runid. Default: Uses the 
#' filename.
#' 
#' @param limit number of documents to parse for each query. 
#' Truncates document beyond this limit.
#' 
#' @param type indicating the type of run. 
#' Currently only supports TREC five column format. 
#'
#' @details
#' The function creates a C++ object representing a set of system runs provided
#' from the \code{runPaths} folder. 
#' 
#' @export
read.runs <- function(runPaths, runids=NULL, limit=1000, type=c("TREC")){
  type <- match.arg(type)
  suppressPackageStartupMessages(require(tools, quietly=T))
  runPaths <- sapply(runPaths, file_path_as_absolute)
  
  if(is.null(runids)) runids <- sapply(runPaths, basename)
  
  if(length(unique(runids)) != length(runPaths)) warning("Duplicate RunIDs found")
    
  new(Runs, runPaths, runids, limit)
}
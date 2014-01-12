#' @title Harmonic Numbers 
#' 
#' @descriptions
#' \code{harmonic_no} returns the a list of harmonic number from 1 to N.
#' 
#' @param a number N to compute the harmonic number 
#' @return a list of N harmonic numbers, where N is the input parameter.
#' @export
#'  
#' @details
#'  The n-th harmonic number is the sum of the reciprocals of the first 
#'  n natural numbers. For more information, refer to the following wikipedia
#'  link: http://en.wikipedia.org/wiki/Harmonic_number.
#'  

harmonic_no <- function(N){

  tmp <- 1/seq(N)
  harmonic_nos <- numeric(N)
  harmonic_nos[1] <- 1
  t <- sapply(2:length(tmp),
              function(i){harmonic_nos[i]<<- harmonic_nos[i-1] + tmp[i]})  
  
  hn <- harmonic_nos
  rm(harmonic_nos)
  
  return(hn)  
}



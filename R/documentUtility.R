# Document Utility Functions

# Ad-hoc Utility Functions 
utility.cumulativeGain <- function(grades){
  return(2^grades - 1)
}

utility.prec <- function(grades){
  cumGrade <- cumsum(grades)
  return(cumGrade * P.Rank(length(grades)))
}

utility.ERRGain <- function(grades, maxGrade=4){
  return((2^grades - 1) / 2^maxGrade)
}


# Diversity Utility Functions
utility.alphaDCG <- function(runMatrix, alpha=0.5){
  cummSTCountMat <- apply(runMatrix, 2, cumsum) - 1
  gainMatrix <- (1 - alpha)^cummSTCountMat * runMatrix
  gainVector <- apply(gainMatrix, 1, sum)
  return(gainVector)
}
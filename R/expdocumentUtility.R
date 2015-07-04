# Document Utility Functions

# Ad-hoc Utility Functions 
utility.cumulativeGain <- function(grades){
  return(2^grades - 1)
}

utility.prec <- function(grades){
  cumGrade <- cumsum(grades)
  return(cumGrade * P.Rank(length(grades)))
}

utility.ERRGain <- function(grades, maxGrade){
  ifelse(maxGrade == 1,return(grades), return((2^grades - 1) / 2^maxGrade))
  #return(grades / (maxGrade+1))
}


# Diversity Utility Functions
utility.alphaDCG <- function(runMatrix, alpha=0.5){
  cummSTCountMat <- apply(runMatrix, 2, cumsum) - 1
  gainMatrix <- (1 - alpha)^cummSTCountMat * runMatrix
  gainVector <- apply(gainMatrix, 1, sum)
  return(gainVector)
}

# Triplet Utility Function
utility.triplet <- function(util, cond, docid, type='ave'){
  # Get the utility score for each condition
  util$conditions <- as.character(util$conditions)
  util$doc <- as.character(util$doc)

  scores <- unlist(sapply(cond, function(x) 
    subset(util, conditions==x & doc == docid[1])$utility))
  if(length(scores) == 0)  return (0)
  else if(type == 'ave') return(sum(scores)/length(cond))
  else if(type == 'min') 
    ifelse(max(scores) == 0, return(0), return(min(scores)))
  
}
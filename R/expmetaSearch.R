metaSearch.metaAP <- function(rank_matrix, 
                              method=c("sum", "mean", "max", "sd"), 
                              N=1000){

  method <- match.arg(method)
  # Pre-compute the harmonic numbers
  harmonic_nos <- c(0, harmonic_no(N))
  
  # Compute the H_N
  H_N <- harmonic_nos[N]
  
  # Compute the H_n
  H_n <- matrix(harmonic_nos[rank_matrix+1], nrow=nrow(rank_matrix))
  rownames(H_n) <- rownames(rank_matrix)
  
  if(method == "sum") mat <- rowSums(1 + H_N - H_n)
  else if(method == "mean") mat <- rowMeans(1 + H_N - H_n)
  else if(method == "max") mat <- apply((1 + H_N - H_n), 1, max)
  else if(method == "sd") mat <- apply((1 + H_N - H_n), 1, sd)
  
  # Clear the created global variable
  return(sort(mat, decreasing=T))
}

metaSearch.combMNZ <- function(score_matrix){
  mat <- rowMeans(1 + H_N - H_n)
  return(sort(mat, decreasing=T))
}


metaSearch.BordaCount <- function(score_matrix){
  mat <- rowMeans(1 + H_N - H_n)
  return(sort(mat, decreasing=T))
}

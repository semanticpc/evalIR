metaAP <- function(rank_matrix, method=c("sum", "mean", "max", "sd")){
  method <- match.arg(method)
  N <- apply(rank_matrix, 2, max)
  
  
  # Pre-compute the harmonic numbers
  tmp <-c(0, 1/seq(max(N)))
  harmonic_nos <- numeric(max(N))
  harmonic_nos[2] <- 1
  t <- sapply(3:length(tmp),
         function(i){harmonic_nos[i]<<- harmonic_nos[i-1] + tmp[i]})
  
  # Compute the H_N
  H_N <- sapply(N, function(n) harmonic_nos[n])
  
  # Compute the H_n
  H_n <- matrix(harmonic_nos[rank_matrix+1], nrow=nrow(rank_matrix))
  rownames(H_n) <- rownames(rank_matrix)
  
  if(method == "sum") mat <- rowSums(1 + H_N - H_n)
  else if(method == "mean") mat <- rowMeans(1 + H_N - H_n)
  else if(method == "max") mat <- apply((1 + H_N - H_n), 1, max)
  else if(method == "sd") mat <- apply((1 + H_N - H_n), 1, sd)
  
  # Clear the created global variable
  #rm(harmonic_nos)
  return(sort(mat, decreasing=T))
}

combMNZ <- function(score_matrix){
  mat <- rowMeans(1 + H_N - H_n)
  return(sort(mat, decreasing=T))
}

metaAP <- function(rank_matrix, method=c("sum", "mean", "max", "sd")){
  method <- match.arg(method)
  N <- apply(rank_matrix, 2, max)
  
  H_N <- sapply(N, function(n) sum(1/1:n))
  
  if(!exists("H_n_values"))
    H_n_values <<- sapply(1:max(N),function(x) sum(1/1:x))
  
  H_n <- apply(rank_matrix, 1:2, function(x) if(x > 0) H_n_values[x] else 0) 
  
  if(method == "sum") mat <- rowSums(1 + H_N - H_n)
  else if(method == "mean") mat <- rowMeans(1 + H_N - H_n)
  else if(method == "max") mat <- apply((1 + H_N - H_n), 1, max)
  else if(method == "sd") mat <- apply((1 + H_N - H_n), 1, sd)
  return(sort(mat, decreasing=T))
}

combMNZ <- function(score_matrix){
  mat <- rowMeans(1 + H_N - H_n)
  return(sort(mat, decreasing=T))
}
# Probability of Stopping at rank k 
P.Rank <- function(rank){
  return(1/(1:rank))
}

P.RBP <- function(rank, beta=0.5){
  return((1 - beta)^(1:rank -1))# * beta)
}

P.LOG <- function(rank, theta=2){
  return((1/log(1:rank + theta - 1, theta)))
}

P.DCG <- function(rank, theta=2){
  return((1/log(1:rank + theta - 1, theta)) -  (1/log(1:rank + theta, theta)))
}

P.RR <- function(rank){
  return( 1 / (1:rank * (1:rank + 1)))
}


# Dynamic stopping probability functions
P.AP <- function(grade_vector, total_rel){
  if(total_rel == 0) return(0)
  else return( grade_vector / total_rel)
}

P.RRR <- function(rank, grade_vector){
  R_k <- cumsum(grade_vector)
  return(sapply(1:rank, function(k, R_k) 
    grade_vector[k] / (R_k[k] * (R_k[k] + 1)), R_k))
}

P.ERR <- function(rank, grade_vector, maxGrade=4, theta=0.5){
  R_k <- cumsum(grade_vector)
  if(maxGrade == 1){
    return(sapply(1:rank, function(k, R_k) 
      grade_vector[k] * (1 - theta)^(R_k[k] - 1), R_k))
  }else {
    # Decay starts from 1 and it is the cumulative product of (1 - gainvalue)
    decay <- c(1, cumprod(1 - grade_vector)[1:rank-1])
    return(grade_vector[1:rank] * decay)
  }
    
}






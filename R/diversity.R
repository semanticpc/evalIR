eval.diversity <- function(qrels, runs, ranks=NULL, query=FALSE){
  maxRank <- max(ranks)
  all_q_res <- data.frame()
  for(run in names(runs)){
    runid <- run
    
    for(q in names(qrels)){
      run <- names(sort(runs$getRankMatrix(q)[,runid][runs$getRankMatrix(q)[,runid] > 0]))
      grades_mat <- qrels$judgeQuery(q, run)
      qrels_grades_mat <- qrels$qrels$getMatrix(q)
      
      header <- c()
      res <- c()
      maxRank <- max(ranks)
      
      header <- c(header, c('runid'))
      res <- c(res, runid)
      
      header <- c(header, c('topic'))
      res <- c(res, q)
      
      # Compute S-Recall      
      res <- c(res, SRecall(qrels[[q]], mat[1:maxRank,], ranks))
      header <- c(header, sapply(ranks,function(x) paste('srecall@',x,sep='')))
      
      res <- c(res, AlphaDCG(qrels[[q]], mat[1:maxRank,], ranks))
      header <- c(header, sapply(ranks,function(x) paste('aDCG@',x,sep='')))
      
      res <- c(res, AlphaNDCG(qrels[[q]], mat[1:maxRank,], ranks))
      header <- c(header, sapply(ranks,function(x) paste('anDCG@',x,sep='')))      
      
      res <- c(res, ERRIA(qrels[[q]], mat[1:maxRank,], ranks))      
      header <- c(header, sapply(ranks,function(x) paste('ERR-IA@',x,sep='')))
      
      res <- c(res, DCGIA(qrels[[q]], mat[1:maxRank,], ranks))
      header <- c(header, sapply(ranks,function(x) paste('DCG-IA@',x,sep='')))
      
      res <- c(res, MAPIA(qrels[[q]], mat))
      header <- c(header, c('MAP-IA'))
      
      res <- c(res, PrecIA(qrels[[q]], mat[1:maxRank,], ranks))      
      header <- c(header, sapply(ranks,function(x) paste('Prec-IA@',x,sep='')))
      
      res <- c(res, NRBP(qrels[[q]], mat))
      header <- c(header, c('NRBP'))
      
      res <- c(res, nNRBP(qrels[[q]], mat))
      header <- c(header, c('nNRBP'))
      
      names(res) <- header
      
      all_q_res <- rbind(all_q_res, t(res))
    }
  }

  if(query)
    all_q_res <- apply(all_q_res, 2, mean);
  
  return(all_q_res);
}

# Evaluation Measures for Novelty and Diversity 

AlphaDCG <- function(qrels, run, ranks, alpha=0.5){
  maxRank <- max(ranks)
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, maxRank)
  

  utility <- utility.alphaDCG(runMatrix)
  discount <- P.LOG (maxRank)
  adcg_values <- cumsum(utility * discount)
  
  # Ideal ideal normalization as done by ndeval
  idealIdealGain <- (1 - alpha)^c(0:(maxRank-1)) * qrels@numOfSubtopics
  norm <- cumsum(idealIdealGain * discount)
  adcg_values <- adcg_values / norm
  
  return(adcg_values[ranks])
}

AlphaNDCG <- function(qrels, run, ranks, alpha=0.5, idealMatrix=NULL){
  
  maxRank <- max(ranks)
  
  # Using a C++ backend obtain the idea subtopic ranking using a greedy approach
  # Adding 1 as R Vectors indexes starts from 1 
  if(is.null(idealMatrix)) idealMatrix <- ideal.alphaDCG(qrels, maxRank)  
    
  ideal.utility <- utility.alphaDCG(idealMatrix)
  ideal.discount <- P.LOG (maxRank)
  ideal.adcg <- cumsum(ideal.utility * ideal.discount)
  
  
  runMatrix <- judge(qrels, run, maxRank)
  run.utility <- utility.alphaDCG(runMatrix)
  run.discount <- P.LOG (maxRank)
  run.adcg <- cumsum(run.utility * run.discount)
  
  andcg_values <- run.adcg / ideal.adcg
  
  return(andcg_values[ranks])
}

SRecall <- function(qrels, run, ranks){
  
  maxRank <- max(ranks)

  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, maxRank)
  
  
  # Using a C++ backend obtain the idea subtopic ranking using a greedy approach
  idealMatrix <- ideal.srecall(qrels, maxRank)
  
  # Count the maximum number of subtopic that can be retrieved at each rank
  subtopicRet_ideal <- sapply(1:maxRank, function(x) 
    sum((apply(data.matrix(idealMatrix[1:x,]), 2, sum) >= 1) * 1))
  
  # Count the number of unique subtopics retrieved at each rank
  subtopicRet_run <- sapply(1:maxRank, function(x) 
    sum((apply(data.matrix(runMatrix[1:x,]), 2, sum) >= 1) * 1))
  
  srecall_values <- subtopicRet_run/subtopicRet_ideal
  srecall_values[is.na(srecall_values)] <- 0
  
  return(srecall_values[ranks])
}

ERRIA <- function(qrels, run, ranks, alpha=0.5){
  maxRank <- max(ranks)
  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(rep(0, maxRank))
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, maxRank)
  
  erria <- sapply(1:ncol(qrels@relDocMatrix), function(st) 
    P.Rank(maxRank) * P.ERR(maxRank, runMatrix[,st], 0.5))
  colnames(erria) <- colnames(runMatrix)
  erria <- erria * qrels@subtopicProbDist
  erria <- cumsum(apply(erria, 1, sum))
  
  
  # Ideal ideal normalization as done by ndeval
  idealIdealGain <- (1 - alpha)^c(0:(maxRank-1)) * qrels@numOfSubtopics
  norm <- cumsum(idealIdealGain * P.Rank(maxRank))
  
  erria <- erria / norm
  
  
  return(erria[ranks])
}

MAPIA <- function(qrels, run, rankLimit=1000){

  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(0)
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, rankLimit)
  
  mapia <- sapply(1:ncol(getSubtopicMatrix(qrels)), function(st) 
    sum(utility.prec(runMatrix[,st]) * P.AP(runMatrix[,st], 
                                            sum(qrels@relDocMatrix[,st]) ) ) )

  names(mapia) <- colnames(getSubtopicMatrix(qrels))
  mapia <- sum(mapia * qrels@subtopicProbDist)
  
  return(mapia)
}

PrecIA <- function(qrels, run, ranks){
  maxRank <- max(ranks)
  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(rep(0, maxRank))
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, maxRank)
  
  precia <- sapply(1:ncol(qrels@relDocMatrix), function(st) 
    utility.prec(runMatrix[,st]))
  
  colnames(precia) <- colnames(runMatrix)
  precia <- precia * qrels@subtopicProbDist
  precia <- apply(precia, 1, sum)
  
  return(precia[ranks])
}

DCGIA <- function(qrels, run, ranks, alpha=0.5){
  maxRank <- max(ranks)
  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(rep(0, maxRank))
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, maxRank)
  
  dcgia <- sapply(1:ncol(qrels@relDocMatrix), function(st) 
    utility.cumulativeGain(runMatrix[,st]) * P.LOG(maxRank))
  
  colnames(dcgia) <- colnames(runMatrix)
  dcgia <- dcgia * qrels@subtopicProbDist
  dcgia <- apply(dcgia, 1, sum)
  
  return(dcgia[ranks])
}

NRBP <- function(qrels, run, alpha=0.5, beta=0.5, rankLimit=1000){

  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(0)
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, rankLimit)
  
  nrbp <-  sum(utility.alphaDCG(runMatrix) *  P.RBP(rankLimit))
  
  nrbp <- nrbp / qrels@numOfSubtopics
  nrbp <- sum(nrbp * (1 - (1 - alpha) * beta))
  
  return(nrbp)
}

nNRBP <- function(qrels, run, alpha=0.5, beta=0.5, idealMatrix=NULL, rankLimit=1000){
  
  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(qrels@numOfSubtopics == 0) return(0)
  
  if(is.null(idealMatrix)) idealMatrix <- ideal.alphaDCG(qrels, rankLimit)  
  
  # Get judged matrix of subtopic presence 
  runMatrix <- judge(qrels, run, rankLimit)
  
  nrbp <-  sum(utility.alphaDCG(runMatrix) *  P.RBP(rankLimit))
  nrbp <- nrbp / qrels@numOfSubtopics
  nrbp <- sum(nrbp * (1 - (1 - alpha) * beta))
  
  
  ideal.nrbp <-  sum(utility.alphaDCG(idealMatrix) *  P.RBP(rankLimit))
  ideal.nrbp <- ideal.nrbp / qrels@numOfSubtopics
  ideal.nrbp <- sum(ideal.nrbp * (1 - (1 - alpha) * beta))
  
  
  return(nrbp/ideal.nrbp)
}

################################################################################
# Ideal Matrices
################################################################################

ideal.alphaDCG <- function(qrels_matrix, rank, alpha=0.5){
  idealRanking <- 1 + .Call("andcg_ideal", 
                            data.matrix(qrels_matrix), 
                            rank, alpha, PACKAGE = "evalIR" )
  idealMatrix <- qrels_matrix[idealRanking, ]
  # Add dummy zero rows to idealMatrix if number of documents is less than rank
  if(nrow(idealMatrix) < rank){
    dummyMatrix <- matrix(0,nrow = rank - nrow(idealMatrix), 
                          ncol = ncol(idealMatrix))
    colnames(dummyMatrix) <- colnames(idealMatrix)
    idealMatrix <- rbind(idealMatrix, dummyMatrix)
  } 
  idealMatrix
 
}

ideal.srecall <- function(qrels, rank){
  idealRanking <- 1 + .Call("srecall_ideal", data.matrix(qrels@relDocMatrix), 
                            rank, PACKAGE = "evalIR" )
  idealMatrix <- qrels@relDocMatrix[idealRanking, ]
  # Add dummy zero rows to idealMatrix if number of documents is less than rank
  if(nrow(idealMatrix) < rank){
    dummyMatrix <- matrix(0,nrow = rank - nrow(idealMatrix), 
                          ncol = ncol(idealMatrix))
    colnames(dummyMatrix) <- colnames(idealMatrix)
    idealMatrix <- rbind(idealMatrix, dummyMatrix)
  } 
  idealMatrix
  
}




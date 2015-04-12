#' @title Diversity Evaluation
#'
#' @description
#' \code{eval.diversity} returns a matrix with the scores returned by various
#' diversity evaluation measures for each system run per topic.
#'
#' @param qrels C++ object returned by \code{read.qrels} with type = 'diversity'
#'
#' @param runs  C++ object returned by \code{read.runs}
#'
#' @param measures a character vector containing a set of evaluation measures.
#'  Accepted measures are c('num_ret', 'simple_prec', srecall', 'AlphaDCG',
#'  'AlphanDCG', 'ERRIA','MAPIA','nDCGIA','NRBP', 'nNRBP').
#' Make sure to use the exact same names.
#'
#' @param ranks A numeric vector contisting of a set of ranks for which scores
#' are to be computed.
#'
#' @details
#' The function is the main entry to obtain the scores for a set of evaluation
#' measures. The returns a matrix with the following headers: runid, queryid,
#' nDCG@10, .... A set of measures can be passed on as argument.
#'
#' @export
#'
eval.diversity <- function(qrels, runs, measures=NULL, decimal = 4,
                           rankCutoff=1000, ranks=NULL,
                           sort=c('trecSort', 'rankSort','ndevalSort')){
  sort <- match.arg(sort)
  maxRank <- max(ranks)
  all_q_res <- data.frame()


  if(is.null(measures)) measures <- c('srecall', 'AlphaDCG', 'AlphanDCG',
                                      'ERRIA','MAPIA','DCGIA','NRBP', 'nNRBP',
                                      'simple_prec')#'num_ret'
  for(runid in runs$getRunids()){
    for(qid in qrels$getQueries()){

      # Run Rank List Sorting
      run <- runSort(runs, qid, runid, sort)

      grades <- qrels$judgeQuery(qid, run)
      grades[grades > 1] <- 1
      grades[grades <= 0] <- 0
      
      
      qrels_grades <- qrels$getMatrix(qid)
      qrels_grades[qrels_grades > 1] <- 1
      qrels_grades[qrels_grades <= 0] <- 0
      stProb <- qrels$getSubtopicProbabilties(qid)

      # Compute Ideal Matrices

      header <- c()
      res <- c()
      maxRank <- max(ranks)

      res <- c(res, runid)
      header <- c(header, c('runid'))

      res <- c(res, qid)
      header <- c(header, c('topic'))

      for(measure in measures){
        if(measure %in% c('AlphaDCG', 'AlphanDCG', 'srecall','simple_prec')){
          res <- c(res, do.call(measure, list(qrels_grades, grades, ranks)))
          header <- c(header, paste0(measure,'@',ranks))
        }

        else if(measure %in% c('ERRIA', 'DCGIA')){
          res <- c(res, do.call(measure, list(qrels_grades, grades,
                                              ranks, subtopicProbDist=stProb)))
          header <- c(header, paste0(measure,'@',ranks))
        }

        else if(measure %in% c('MAPIA')){
          res <- c(res, do.call(measure, list(qrels_grades, grades, rankCutoff,
                                              subtopicProbDist=stProb/length(stProb))))
          header <- c(header, measure)
        }

        else if(measure %in% c('NRBP', 'nNRBP')){
          res <- c(res, do.call(measure, list(qrels_grades, grades, rankCutoff)))
          header <- c(header, measure)
        }
        else if(measure %in% c('num_ret')){
          res <- c(res, do.call(measure, list(grades, rankCutoff)))
          header <- c(header, measure)
        }


      }

      names(res) <- header
      all_q_res <- rbind(all_q_res, t(res))
    }
  }
  # Round Scores to 'decimal' point
  numOfCols <- ncol(all_q_res)
  all_q_res[,3:numOfCols] <- sapply(all_q_res[,3:numOfCols], function(x)
    round(as.numeric(as.character(x)), decimal))

  return(all_q_res)
}

#' @title Number of Retrieved Documents
#'
#' @description
#' \code{num_ret} returns the number of documents retrieved.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param rankLimit document cut off
#'
num_ret.matrix <- function(grades, rankLimit=1000){
  if(rankLimit <= nrow(grades)) grades <- grades[1:rankLimit,]
  return(nrow(grades))
}

#' @title Simple Precision
#'
#' @description
#' \code{simple_prec} returns the Precision at rank k. Even if the document
#' contains a single relevant subtopic the document is relevant.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param rankLimit document cut off
#'
simple_prec <- function(qrels_grades, grades, ranks){
  grades <- rowSums(grades)
  return(Prec(NULL, grades, ranks))
}



#' @title Alpha - Discounted Cumulative Gain at k
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
AlphaDCG <- function(qrels_grades, grades, ranks, idealMatrix=NULL,
                     alpha=0.5){

  maxRank <- max(ranks)
  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])


  utility <- utility.alphaDCG(grades, alpha)
  discount <- P.LOG (maxRank)
  adcg_values <- cumsum(utility * discount)

  # Ideal ideal normalization as done by ndeval
  idealIdealGain <- (1 - alpha)^c(0:(maxRank-1)) * numOfSubtopics
  norm <- cumsum(idealIdealGain * discount)
  adcg_values <- adcg_values / norm
  names(adcg_values) <- c()
  return(adcg_values[ranks])
}


#' @title Alpha - Normalized Discounted Cumulative Gain at k
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
AlphanDCG <- function(qrels_grades, grades, ranks, idealMatrix=NULL, alpha=0.5){

  maxRank <- max(ranks)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])

  # Using a C++ backend obtain the ideal subtopic ranking using a greedy approach
  if(is.null(idealMatrix)) idealMatrix <- ideal.alphaDCG(qrels_grades, maxRank)


  # Calculation of Ideal Score
  ideal.utility <- utility.alphaDCG(idealMatrix)
  ideal.discount <- P.LOG (maxRank)
  ideal.adcg <- cumsum(ideal.utility * ideal.discount)

  # Calculation of System Score
  run.utility <- utility.alphaDCG(grades)
  run.discount <- P.LOG (maxRank)
  run.adcg <- cumsum(run.utility * run.discount)

  andcg_values <- run.adcg / ideal.adcg
  names(andcg_values) <- c()

  return(andcg_values[ranks])
}


#' @title Subtopic Recall
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
srecall <- function(qrels_grades, grades, ranks, idealMatrix=NULL){

  maxRank <- max(ranks)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])

  # Using a C++ backend obtain the idea subtopic ranking using a greedy approach
  if(is.null(idealMatrix)) idealMatrix <- ideal.srecall(qrels_grades, maxRank)

  # Count the maximum number of subtopic that can be retrieved at each rank
  ideal.subtopicCount <- sapply(1:maxRank, function(x)
    sum((apply(data.matrix(idealMatrix[1:x,]), 2, sum) >= 1) * 1))

  # Count the number of unique subtopics retrieved by the System
  run.subtopicCount <- sapply(1:maxRank, function(x)
    sum((apply(data.matrix(grades[1:x,]), 2, sum) >= 1) * 1))

  srecall_values <- run.subtopicCount/ideal.subtopicCount
  srecall_values[is.na(srecall_values)] <- 0
  names(srecall_values) <- c()

  return(srecall_values[ranks])
}


#' @title Expected Reciprocal Rank - Intent Aware at k
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
ERRIA <- function(qrels_grades, grades, ranks, subtopicProbDist=NULL,
                  alpha=0.5){

  maxRank <- max(ranks)
  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])


  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(rep(0, maxRank))

  if(is.null(subtopicProbDist))
    subtopicProbDist <- rep(1/numOfSubtopics, numOfSubtopics)


  subtopics <- 1:ncol(qrels_grades)
  subtopics <- subtopics[colSums(qrels_grades) > 0]
  erria <- sapply(subtopics, function(st)
    ERR(NULL,grades[,st], 1:maxRank, maxGrade=1))

  #erria <- erria * subtopicProbDist
  erria <- rowSums(erria)


  # Ideal ideal normalization as done by ndeval
  idealIdealGain <- (1 - alpha)^c(0:(maxRank-1)) * numOfSubtopics
  norm <- cumsum(idealIdealGain * P.Rank(maxRank))

  erria <- erria / norm
  names(erria) <- c()

  return(erria[ranks])
}


#' @title Mean Average Precision - Intent Aware
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
MAPIA <- function(qrels_grades, grades, rankLimit=1000, subtopicProbDist=NULL){

  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < rankLimit) grades <- fill.matrix(grades, rankLimit)
  grades <- data.matrix(grades[1:rankLimit,])

  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(rep(0, maxRank))

  if(is.null(subtopicProbDist))
    subtopicProbDist <- rep(1/numOfSubtopics, numOfSubtopics)

  subtopics <- 1:ncol(qrels_grades)
  subtopics <- subtopics[colSums(qrels_grades) > 0]
  mapia <- sapply(subtopics, function(st)
    sum(utility.prec(grades[,st]) * P.AP(grades[,st], sum(qrels_grades[,st]))))


  mapia <- sum(mapia * subtopicProbDist)
  names(mapia) <- c()

  return(mapia)
}


#' @title Precision - Intent Aware
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
PrecIA <- function(qrels_grades, grades, ranks, subtopicProbDist=NULL){

  maxRank <- max(ranks)
  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])

  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(rep(0, maxRank))

  if(is.null(subtopicProbDist))
    subtopicProbDist <- rep(1/numOfSubtopics, numOfSubtopics)

  subtopics <- 1:ncol(qrels_grades)
  subtopics <- subtopics[colSums(qrels_grades) > 0]
  precia <- sapply(subtopics, function(st) utility.prec(grades[,st]))

  precia <- precia * subtopicProbDist
  precia <- rowSums(precia)
  names(precia) <- c()

  return(precia[ranks])
}


#' @title Discounted Cumulative Gain - Intent Aware
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
DCGIA <- function(qrels_grades, grades, ranks, subtopicProbDist=NULL,
                  alpha=0.5){

  maxRank <- max(ranks)
  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < maxRank) grades <- fill.matrix(grades, maxRank)
  grades <- data.matrix(grades[1:maxRank,])

  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(rep(0, maxRank))

  if(is.null(subtopicProbDist)) subtopicProbDist <- rep(1, numOfSubtopics)

  dcgia <- sapply(1:numOfSubtopics, function(st)
    utility.cumulativeGain(grades[,st]) * P.LOG(maxRank))


  dcgia <- dcgia * subtopicProbDist
  dcgia <- apply(dcgia, 1, sum)
  names(dcgia) <- c()

  return(dcgia[ranks])
}


#' @title Novelty and Rank Biased Precision
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
NRBP <- function(qrels_grades, grades, rankLimit=1000, idealMatrix=NULL,
                 alpha=0.5, beta=0.5){

  # We don't use the idealMatrix in this function at all
  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < rankLimit) grades <- fill.matrix(grades, rankLimit)
  grades <- data.matrix(grades[1:rankLimit,])



  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(0)

  nrbp <-  sum(utility.alphaDCG(grades) *  P.RBP(rankLimit))

  nrbp <- nrbp / numOfSubtopics
  nrbp <- sum(nrbp * (1 - (1 - alpha) * beta))

  return(nrbp)
}


#' @title Normalized Novelty and Rank Biased Precisionf
#'
#' @description
#' \code{AP}  Precision
#'
#' @param qrels_grades a numeric matrix containing the grades all the judged
#' documents in the qrels file.
#'
#' @param grades a numeric matrix of the grades.
#'
#' @param ranks a numeric vector containing a set of ranks at which the measure
#' is calculated.
#'
#' @export
nNRBP <- function(qrels_grades, grades, rankLimit=1000, idealMatrix=NULL,
                  alpha=0.5, beta=0.5){

  numOfSubtopics <- sum(colSums(qrels_grades) > 0)
  if(nrow(grades) < rankLimit) grades <- fill.matrix(grades, rankLimit)
  grades <- data.matrix(grades[1:rankLimit,])



  # If numOfSubtopics is zero no relevant documents for any subtopic
  if(numOfSubtopics == 0) return(0)

  if(is.null(idealMatrix)) idealMatrix <- ideal.alphaDCG(qrels_grades, rankLimit)


  nrbp <-  sum(utility.alphaDCG(grades) *  P.RBP(rankLimit))
  nrbp <- nrbp / numOfSubtopics
  nrbp <- sum(nrbp * (1 - (1 - alpha) * beta))


  ideal.nrbp <-  sum(utility.alphaDCG(idealMatrix) *  P.RBP(rankLimit))
  ideal.nrbp <- ideal.nrbp / numOfSubtopics
  ideal.nrbp <- sum(ideal.nrbp * (1 - (1 - alpha) * beta))


  return(nrbp/ideal.nrbp)
}

################################################################################
# Ideal Matrices
################################################################################

ideal.alphaDCG <- function(qrels_matrix, rank, alpha=0.5){
  relDocs <- sum(rowSums(qrels_matrix) > 0)
  idealRanking <- 1 + .Call("andcg_ideal",data.matrix(qrels_matrix),
                            min(relDocs, rank), alpha, PACKAGE = "evalIR" )

  idealMatrix <- data.matrix(qrels_matrix[idealRanking, ])
  # Add dummy zero rows to idealMatrix if number of documents is less than rank
  if(nrow(idealMatrix) < rank) idealMatrix <- fill.matrix(idealMatrix, rank)
  idealMatrix

}

ideal.srecall <- function(qrels_matrix, rank){
  relDocs <- sum(rowSums(qrels_matrix) > 0)
  idealRanking <- 1 + .Call("srecall_ideal", data.matrix(qrels_matrix),
                            min(relDocs, rank), PACKAGE = "evalIR" )
  idealMatrix <- data.matrix(qrels_matrix[idealRanking, ])
  # Add dummy zero rows to idealMatrix if number of documents is less than rank
  if(nrow(idealMatrix) < rank) idealMatrix <- fill.matrix(idealMatrix, rank)

  idealMatrix

}

fill.matrix <- function(mat, rank){
  dummyMatrix <- matrix(0,nrow = rank - nrow(mat),
                        ncol = ncol(mat))
  colnames(dummyMatrix) <- colnames(mat)
  return (rbind(mat, dummyMatrix))
}






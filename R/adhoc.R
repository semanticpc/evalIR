#' @title Ad-hoc Evaluation
#'
#' @description
#' \code{eval.adhoc} returns a matrix with the scores returned by the evaluation
#' measures for each system run per topic.
#' 
#' @param qrels C++ object returned by \code{read.qrels}
#' 
#' @param runs  C++ object returned by \code{read.runs}
#' 
#' @param measures a character vector containing a set of evaluation measures. 
#'  Accepted measures are c('num_ret', 'num_rel', 'num_rel_ret', 'recall','AP
#' ,'recip_rank','Rprec', 'Prec', 'DCG', 'nDCG', 'ERR', 'nERR'). 
#' Make sure to use the exact same names.
#'
#' @param ranks A numeric vector contisting of a set of ranks for which scores 
#' are to be computed. Applies only to certain measure that needs a cut off like
#' Prec@k, nDCG@k, ERR@k, etc.
#'  
#' @details
#' The function is the main entry to obtain the scores for a set of evaluation
#' measures. The returns a matrix with the following headers: runid, queryid,
#' nDCG@10, .... A set of measures can be passed on as argument. 
#' 
#' @export
eval.adhoc <- function(qrels, runs, measures=NULL, decimal = 4,
                       ranks=NULL, sort=c('trecSort', 'rankSort')){
  sort <- match.arg(sort)
  maxRank <- max(ranks)
  all_q_res <- data.frame()
  
  
  if(is.null(measures)) measures <- c('num_ret', 'num_rel', 'num_rel_ret',
                                      'recall','AP','recip_rank','Rprec',
                                      'Prec', 'DCG', 'nDCG', 'ERR', 'nERR')
  for(runid in runs$getRunids()){
    for(q in qrels$getQueries()){
      
      # Run Rank List Sorting 
      run <- runSort(runs, q, runid, sort)

      if(is.null(run)) next
      
      grades <- qrels$judgeQuery(q, run)
      qrels_grades <- qrels$getGrades(q)
      header <- c()
      res <- c()
      maxRank <- max(ranks)

      res <- c(res, runid)
      header <- c(header, c('runid'))
      
      res <- c(res, q)
      header <- c(header, c('topic'))
      
      for(measure in measures){
        if(measure %in% c('DCG', 'nDCG', 'ERR', 'nERR')){
          
          res <- c(res, do.call(measure, list(qrels_grades, grades[1:maxRank],
                                              ranks)))
          header <- c(header, paste0(measure, '@',ranks))
        }
        else if(measure %in% c('Prec')){
          res <- c(res, do.call(measure, list(qrels_grades, grades, ranks)))
          header <- c(header, paste0(measure, '@',ranks))
                      
        }
        else if(measure %in% c('recip_rank', 'num_ret')){
          res <- c(res, do.call(measure, list(grades)))
          header <- c(header, c(measure))
        }else {
          res <- c(res, do.call(measure, list(qrels_grades, grades)))
          header <- c(header, c(measure))
        } 
        
      }
        
      names(res) <- header
      all_q_res <- rbind(all_q_res, t(res))
    }
  }
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
#' @param grades a numeric vector of the grades. 
#' 
#' @param rankLimit document cut off 
#' 
num_ret.numeric <- function(grades, rankLimit=1000){
  grades <- grades[1:rankLimit]
  return(sum(!is.na(grades)))
}


#' @title Number of Relevant Documents
#'
#' @description
#' \code{num_rel} returns the number of documents retrieved.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @export
num_rel <- function(qrels_grades){
  qrels_grades <- qrels_grades
  return(sum(!is.na(qrels_grades[qrels_grades >= 1])))
}


#' @title Number of Relevant Documents Retrieved
#'
#' @description
#' \code{num_rel_ret} returns the number of documents retrieved.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param rankLimit document cut off 
#' 
#' @export
num_rel_ret <- function(grades, rankLimit=1000){
  grades <- grades[1:rankLimit]
  return(sum(!is.na(grades[grades >= 1])))
}



#' @title Reciprocal Rank
#'
#' @description
#' \code{recip_rank} reciprocal rank.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param rankLimit document cut off 
#' 
#' @export
recip_rank <- function(grades, rankLimit=1000){
  
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  minRelRank <- 0
  if(sum(grades) == 0) return(0)
  else minRelRank <- which.min(! grades )
  
  #utility <- cumsum(grades[1:minRelRank])
  #discount <- P.RR(minRelRank)
  #rr <- sum(utility * discount)
  rr <- 1 / minRelRank 
  return(rr)
}


#' @title Recall
#'
#' @description
#' \code{recall} 
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param rankLimit document cut off 
#' 
#' @export
recall <- function(qrels_grades, grades, rankLimit=1000){
  # Comupte the total number of relevant documents
  total_relevant <- num_rel(qrels_grades)
  total_relevant_retreived <- num_rel_ret(grades, rankLimit)
  return(total_relevant_retreived/ total_relevant)
}


Rprec <- function(qrels_grades, grades, rankLimit=1000){
  total_rel <- num_rel(qrels_grades)
  return(Prec(qrels_grades, grades, total_rel))
}

InterpolatedPrec <- function(qrels_grades, grades, rankLimit=1000){  
  
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  recallPts <- floor(seq(0,1,.1) * num_rel(qrels_grades))
  
  # Max Precision at relevant 
  gradesTable <- table(cumsum(grades))
  precs <- as.numeric(names(gradesTable)) / 
    (cumsum(gradesTable) - as.vector(gradesTable - 1))
  
  iprec <- sapply(1:11, function(x) {
    lower <- recallPts[x]
    upper <- min(length(gradesTable), recallPts[x+1])
    if(is.na(lower) || is.na(upper)) return(0)
    else return(max(precs[lower:upper]))
  })
  
  iprec[is.na(iprec)] <- 0
  return(iprec)
}

#' @title Mean Average Precision
#'
#' @description
#' \code{AP} Average Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param rankLimit document cut off 
#' 
#' @export
AP <- function(qrels_grades, grades, rankLimit=1000){
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  total_relevant <- num_rel(qrels_grades)
  
  utility <- Prec(qrels_grades, grades, 1:rankLimit)
  discount <- P.AP(grades, total_relevant)[1: rankLimit]
  
  discount[is.na(discount)] <- 0
  
  ap <- sum(utility * discount)
  return(ap)
}


#' @title Precision at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
Prec <- function(qrels_grades, grades, ranks){
  grades <- grades[1:max(ranks)]
  grades[is.na(grades)] <- 0
  
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  # Compute Precision at various ranks up until max rank in 'ranks' arg
  return(sapply(ranks, function(k) sum(grades[1:k]) / k))
}


#' @title Graded Precision at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
GradedPrec <- function(qrels_grades, grades, ranks){
  
  # Compute Precision at various ranks up until max rank in 'ranks' arg
  maxRank <- max(ranks)
  return(sapply(1:rank, function(k) sum(grades[1:k]) / k))
}



#' @title Expected Reciprocal Rank at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
ERR <- function(qrels_grades, grades, ranks, maxGrade=4){
  grades <- grades[1:max(ranks)]
  grades <- utility.ERRGain(grades, maxGrade)
  
  one_divded_by_rank <- P.Rank(max(ranks))
  discount <- P.ERR(max(ranks), grades, maxGrade)
  
  err <- cumsum(one_divded_by_rank * discount)
  names(err) <- c()
  return(err[ranks])
}

#' @title Normalized Expected Reciprocal Rank at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
nERR <- function(qrels_grades, grades, ranks, maxGrade=4){
  grades <- grades[1:max(ranks)]
  grades <- utility.ERRGain(grades, maxGrade)
  
  
  ideal.grades <- sort(qrels_grades, decreasing=T)[1:max(ranks)]
  ideal.grades[is.na(ideal.grades)] <- 0
  ideal.grades <- utility.ERRGain(ideal.grades, maxGrade)
  
  ideal.utility <-  P.Rank(max(ranks))
  ideal.discount <- P.ERR(max(ranks), ideal.grades)
  
  utility <- P.Rank(max(ranks))
  discount <- P.ERR(max(ranks), grades)
  
  err <- cumsum(utility * discount)
  ideal.err <- cumsum(ideal.utility * ideal.discount)
  nerr <- (err/ideal.err)[ranks]
  nerr[is.na(nerr)] <- 0
  names(nerr) <- c()
  return(nerr)
}

#' @title Discounted Cumulative Gain at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
DCG <- function(qrels_grades, grades, ranks){
  grades <- grades[1:max(ranks)]
  utility <- utility.cumulativeGain(grades)
  discount <- P.LOG(max(ranks))
  
  dcg <- cumsum(utility * discount)
  names(dcg) <- c()
 return(dcg[ranks])
}

#' @title Normalized Discounted Cumulative Gain at k
#'
#' @description
#' \code{AP}  Precision
#' 
#' @param qrels_grades a numeric vector containing the grades all the judged 
#' documents in the qrels file.
#' 
#' @param grades a numeric vector of the grades. 
#' 
#' @param ranks a numeric vector containing a set of ranks at which the measure 
#' is calculated.
#' 
#' @export
nDCG <- function(qrels_grades, grades, ranks){
  grades <- grades[1:max(ranks)]
  
  ideal.grades <- sort(qrels_grades, decreasing=T)[1:max(ranks)]
  ideal.grades[is.na(ideal.grades)] <- 0
    
  ideal.utility <- utility.cumulativeGain(ideal.grades)
  utility <- utility.cumulativeGain(grades)
  
  discount <- P.LOG(max(ranks))
  
  dcg <- cumsum(utility * discount)
  ideal.dcg <- cumsum(ideal.utility * discount)
  
  ndcg <- (dcg/ideal.dcg)[ranks]
  ndcg[is.na(ndcg)] <- 0
  names(ndcg) <- c()
  return(ndcg)
}

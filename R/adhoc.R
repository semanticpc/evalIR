eval.adhoc <- function(qrels, runs, measures=NULL, ranks=NULL, query=FALSE){
  roundPoint <- 4
  maxRank <- max(ranks)
  all_q_res <- data.frame()
  if(is.null(measures)) measures <- c('num_ret', 'num_rel', 'num_rel_ret',
                                      'Recall','AP','recip_rank','R-Prec',
                                      'Prec', 'DCG', 'nDCG', 'ERR', 'nERR')
  for(runid in runs$getRunids()){
    for(q in qrels$getQueries()){
      run <- names(sort(runs$getRankMatrix(q)[,runid][runs$getRankMatrix(q)[,runid] > 0]))
      grades <- qrels$judgeQuery(q, run)
      qrels_grades <- qrels$getGrades(q)
      header <- c()
      res <- c()
      maxRank <- max(ranks)

      res <- c(res, runid)
      header <- c(header, c('runid'))
      
      res <- c(res, q)
      header <- c(header, c('topic'))
      
      if('num_ret' %in% measures){
        res <- c(res, num_ret(grades))
        header <- c(header, c('num_ret'))
      }
      if('num_rel' %in% measures){
        res <- c(res, num_rel(qrels_grades))
        header <- c(header, c('num_rel'))
      }
      if('num_rel_ret' %in% measures){
        res <- c(res, num_rel_ret(grades))
        header <- c(header, c('num_rel_ret'))
      }
      if('Recall' %in% measures){
        res <- c(res, round(recall(qrels_grades, grades), roundPoint))
        header <- c(header, c('Recall'))  
      }
      if('AP' %in% measures){
        res <- c(res, round(AP(qrels_grades, grades), roundPoint))
        header <- c(header, c('AP'))
      }
      if('recip_rank' %in% measures){
        res <- c(res, round(ReciprocalRank(grades), roundPoint))
        header <- c(header, c('recip_rank'))
      }
      if('R-Prec' %in% measures){
        res <- c(res, round(RPrec(qrels_grades, grades), roundPoint))      
        header <- c(header, c('R-Prec'))
      }
      if('Prec' %in% measures){
        res <- c(res, round(Prec(qrels_grades, grades, ranks), roundPoint))      
        header <- c(header, sapply(ranks,function(x) paste('Prec@',x,sep='')))      
      }

      if('DCG' %in% measures){
        res <- c(res, round(DCG(qrels_grades, grades[1:maxRank], ranks), roundPoint))      
        header <- c(header, sapply(ranks,function(x) paste('DCG@',x,sep='')))
      }
      
      if('ERR' %in% measures){
        res <- c(res, round(ERR(qrels_grades, grades[1:maxRank], ranks), roundPoint))      
        header <- c(header, sapply(ranks,function(x) paste('ERR@',x,sep='')))
      }
      if('nDCG' %in% measures){
        res <- c(res, round(NDCG(qrels_grades, grades[1:maxRank], ranks), roundPoint))      
        header <- c(header, sapply(ranks,function(x) paste('nDCG@',x,sep='')))
      }
      
      if('nERR' %in% measures){
        res <- c(res, round(nERR(qrels_grades, grades[1:maxRank], ranks), roundPoint))      
        header <- c(header, sapply(ranks,function(x) paste('nERR@',x,sep='')))        
      }


      # res <- c(res, RBP(qrels[[q]], grades))
      # header <- c(header, c('RBP'))      
      
      #res <- c(res, InterpolatedPrec(qrels[[q]], grades))      
      #header <- c(header, sapply(0:10,function(x) paste('iPrec@R',x/10,sep='')))      
  
      names(res) <- header
      all_q_res <- rbind(all_q_res, t(res))
    }
  }
   if(!query){
     nums <- ! names(all_q_res) %in% c("topic", "runid")
     all_q_res[,nums] <- sapply(all_q_res[,nums], function(x) as.numeric(as.character(x)))
     mean_q_res <- aggregate(all_q_res[,!names(all_q_res) %in% c("runid",
                                                                "topic",
                                                                "num_ret",
                                                                "num_rel",
                                                                "num_rel_ret")],
                            by=list(all_q_res[,"runid"]), mean)

     num_q_res <- aggregate(all_q_res[,names(all_q_res) %in% c("num_ret",
                                                                "num_rel",
                                                                "num_rel_ret")],
                            by=list(all_q_res[,"runid"]), sum)
     all_q_res <- cbind(runid =mean_q_res[,1], 
                        topic=rep("all", nrow(mean_q_res)), 
                        subset(num_q_res, select = -c(1) ),
                        subset(mean_q_res, select = -c(1) ))
     nums <- sapply(all_q_res, is.numeric)
     all_q_res[,nums] <- round(all_q_res[,nums], 4)
   }
     


  return(all_q_res)
}

num_ret <- function(grades, rankLimit=1000){
  return(length(grades))
}

num_rel <- function(qrels_grades, rankLimit=1000){
  return(length(qrels_grades))
}

num_rel_ret <- function(grades, rankLimit=1000){
  return(sum(grades >= 1))
}

Prec <- function(qrels_grades, grades, ranks){
  grades <- grades[1:max(ranks)]
  grades[is.na(grades)] <- 0
  
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  # Compute Precision at various ranks up until max rank in 'ranks' arg
  return(sapply(ranks, function(k) sum(grades[1:k]) / k))
}

RPrec <- function(qrels_grades, grades){
  total_rel <- num_rel(qrels_grades)
  return(Prec(qrels_grades, grades, total_rel))
}

GradedPrec <- function(qrels_grades, grades, ranks){
  
  # Compute Precision at various ranks up until max rank in 'ranks' arg
  maxRank <- max(ranks)
  return(sapply(1:rank, function(k) sum(grades[1:k]) / k))
}

recall <- function(qrels_grades, grades, rankLimit=1000){
  # Comupte the total number of relevant documents
  total_relevant <- num_rel(qrels_grades, rankLimit)
  total_relevant_retreived <- num_rel_ret(grades, rankLimit)
  return(total_relevant_retreived/ total_relevant)
}

AP <- function(qrels_grades, grades, rankLimit=1000){
  # Convert Grades to Binary
  grades <- (grades >= 1) * 1
  
  total_relevant <- num_rel(qrels_grades, rankLimit)
  
  utility <- Prec(qrels_grades, grades, 1:rankLimit)
  discount <- P.AP(grades, total_relevant)[1: rankLimit]
  
  discount[is.na(discount)] <- 0
  
  ap <- sum(utility * discount)
  return(ap)
}

ReciprocalRank <- function(grades, rankLimit=1000){
  
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

ERR <- function(qrels_grades, grades, ranks){
  
  grades <- utility.ERRGain(grades)
  
  utility <- P.Rank(max(ranks))
  discount <- P.ERR(max(ranks), grades)
  
  err <- cumsum(utility * discount)
  return(err[ranks])
}

nERR <- function(qrels_grades, grades, ranks){
  ideal.grades <- sort(qrels_grades, decreasing=T)[1:max(ranks)]
  
  ideal.utility <-  P.Rank(max(ranks))
  ideal.discount <- P.ERR(max(ranks), ideal.grades)
  
  utility <- P.Rank(max(ranks))
  discount <- P.ERR(max(ranks), grades)
  
  err <- cumsum(utility * discount)
  ideal.err <- cumsum(ideal.utility * ideal.discount)
  nerr <- (err/ideal.err)[ranks]
  nerr[is.na(nerr)] <- 0
  return(nerr)
}

DCG <- function(qrels_grades, grades, ranks){
  
  utility <- utility.cumulativeGain(grades)
  discount <- P.LOG(max(ranks))
  
  dcg <- cumsum(utility * discount)
  return(dcg[ranks])
}

NDCG <- function(qrels_grades, grades, ranks){
  ideal.grades <- sort(qrels_grades, decreasing=T)[1:max(ranks)]
  
  ideal.utility <- utility.cumulativeGain(ideal.grades)
  utility <- utility.cumulativeGain(grades)
  
  discount <- P.LOG(max(ranks))
  
  dcg <- cumsum(utility * discount)
  ideal.dcg <- cumsum(ideal.utility * discount)
  
  ndcg <- (dcg/ideal.dcg)[ranks]
  ndcg[is.na(ndcg)] <- 0
  
  return(ndcg)
}

infAP<- function(qrels, run, ranks){
  
}

bpref<- function(qrels, run, ranks){
  
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

iprec_at_recall <- function(qrels, run, ranks){
  
}
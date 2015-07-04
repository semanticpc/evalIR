eval.utility <- function(trels, runs, measures=NULL, decimal = 4,
                       ranks=NULL, sort=c('trecSort', 'rankSort', 'ndevalSort')){
  sort <- match.arg(sort)
  maxRank <- max(ranks)
  all_q_res <- data.frame()


  if(is.null(measures)) measures <- c('DCG_ave', 'RR_ave','RBP_ave', 'None_ave',
                                      'DCG_min', 'RR_min','RBP_min', 'None_min')

  for(qid in trels$getQueries()){
    qrels_utility <- getTripletUtility(trels, qid)
    #ideal.ranking <- getIdealRanking(trels, qid, maxRank)
    ideal.utils <- getIdealUtils(qrels_utility, maxRank)
    for(runid in runs$getRunids()){

      # Run Rank List Sorting
      run <- runSort(runs, qid, runid, sort)

      if(is.null(run)) next

      header <- c()
      res <- c()
      maxRank <- max(ranks)

      res <- c(res, runid)
      header <- c(header, c('runid'))

      res <- c(res, qid)
      header <- c(header, c('topic'))

      for(measure in measures){
        P_k <- strsplit(measure, split = '_')[[1]][1]
        F_type <- strsplit(measure, split = '_')[[1]][2]
        res <- c(res, DivTripletMeasure(qrels_utility, run, ranks, ideal.utils, F_type, P_k))
        header <- c(header, c(measure))
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


DivTripletMeasure <- function(qrel_utilities, run, ranks, ideal.utility,
                              F_type=c('ave','min'),
                              P_k=c('RBP', 'DCG', 'RR')){

  maxRank <- max(ranks)
  run <- run[1:maxRank]
  # Compute document utility U(d_i|S)
  if(F_type == 'ave'){
    first_doc <-  utility.triplet(qrel_utilities, c(''), run[1], 'ave')
    run.utility <- sapply(2:length(run), function(x)
      utility.triplet(qrel_utilities, c('',run[1:(x-1)]), run[x], 'ave'))
    run.utility <- c(first_doc, run.utility)

#     ideal_first_doc <- utility.triplet(qrel_utilities, c(''), ideal.ranking[1], 'ave')
#     ideal.utility <- sapply(2:length(ideal.ranking), function(x)
#       utility.triplet(qrel_utilities, c('',ideal.ranking[1:(x-1)]), ideal.ranking[x], 'ave'))
#     ideal.utility <- c(ideal_first_doc, ideal.utility)
  }
  else if(F_type == 'min'){
    first_doc <-  utility.triplet(qrel_utilities, c(''), run[1], 'min')
    run.utility <- sapply(2:length(run), function(x)
      utility.triplet(qrel_utilities, c('',run[1:(x-1)]), run[x], 'min'))
    run.utility <- c(first_doc, run.utility)

#     ideal_first_doc <- utility.triplet(qrel_utilities, c(''), ideal.ranking[1], 'min')
#     ideal.utility <- sapply(2:length(ideal.ranking), function(x)
#       utility.triplet(qrel_utilities, c('',ideal.ranking[1:(x-1)]), ideal.ranking[x], 'min'))
#     ideal.utility <- c(ideal_first_doc, ideal.utility)

  }

  # Compute Discount or P_k()
  if(P_k == 'RBP'){
    beta <- 0.5
    run.discount <- ((1 - beta)^(1:maxRank -1) * beta)
  }
  else if(P_k == 'DCG')
    run.discount <- P.DCG (maxRank)
  else if(P_k == 'RR')
    run.discount <- P.RR (maxRank)
  else if(P_k == 'None')
    run.discount <- 1

  run.prfscore <- cumsum(run.discount * run.utility)
  ideal.prfscore <- cumsum(run.discount * ideal.utility)


  run.prfscore <- cumsum(run.prfscore) / cumsum(ideal.prfscore)
  names(run.prfscore) <- c()

  return(run.prfscore[ranks])

}




# Some helper functions
getIdealUtils <- function(utils, rankLimit) {
  return(sort(utils$utility,decreasing = T)[1:rankLimit])

}
getIdealRanking <- function(qrel_ptr, query, rankLimit, F_type='ave'){
  docs <- qrel_ptr$getDocuments(query)
  conditions <- qrel_ptr$getConditions(query)

  computeScore <- function(cond_doc){
    cond_doc <- as.character(cond_doc)
    conds <- rep(cond_doc, length(docs))
    appearance <- qrel_ptr$getCondApperanceCount(query, conds, docs)
    prefCount <- qrel_ptr$getCondPrefCount(query, conds, docs)
    return(data.frame(doc=docs,utility=prefCount/(appearance+2)))
  }



  ideal_utility <- c()

  simple_prf <- computeScore('')
  first_doc_util <- simple_prf[order(simple_prf$utility,decreasing = T),][1,2]
  first_doc <- simple_prf[order(simple_prf$utility,decreasing = T),][1,1]
  ideal_utility <- rbind(ideal_utility, data.frame(doc=first_doc,
                                                   util=first_doc_util))

#   second_prf <- computeScore(first_doc)
#   second_doc_util <- second_prf[order(second_prf$utility,decreasing = T),][1,2]
#   second_doc <- second_prf[order(second_prf$utility,decreasing = T),][1,1]
#   ideal_utility <- rbind(ideal_utility, data.frame(doc=second_doc,
#                                                    util=second_doc_util))


  getPrfCountMatrix <- function(doc_pool, idealRanking){
    mat <- c()
    for(idoc in idealRanking){
      conds <- rep(idoc, length(doc_pool))
      appearance <- qrel_ptr$getCondApperanceCount(query, conds, doc_pool)
      prefCount <- qrel_ptr$getCondPrefCount(query, conds, doc_pool)
      mat <- cbind(mat, prefCount/(appearance+2))
    }
    mat <- cbind(mat, min=apply(mat, 1, min))
    mat <- cbind(mat, ave=apply(mat, 1, mean))

    return(mat)
  }


  for(rank_index in 2:rankLimit){

    doc_pool <- setdiff(docs, ideal_utility$doc)
    doc_prf <- getPrfCountMatrix(doc_pool, ideal_utility$doc)

    if(F_type == 'min'){
      doc_util <- doc_prf[order(doc_prf[,'min'],decreasing = T),][1,'min']
      doc <- row.names(doc_prf[order(doc_prf[,'min'],decreasing = T),])[1]
    }else if(F_type == 'ave'){
      doc_util <- doc_prf[order(doc_prf[,'ave'],decreasing = T),][1,'ave']
      doc <- row.names(doc_prf[order(doc_prf[,'ave'],decreasing = T),])[1]
    }

    ideal_utility <- rbind(ideal_utility, data.frame(doc=doc,
                                                     util=doc_util))

  }
  return(ideal_utility$doc)

}


getTripletUtility <- function(qrel_ptr, query, docs=NULL){

  if(is.null(docs)){
    docs <- qrel_ptr$getDocuments(query)
    conditions <- qrel_ptr$getConditions(query)
  }else{
    conditions <- c('', docs)
  }

  computeScore <- function(cond_doc){
    cond_doc <- as.character(cond_doc$conditions)
    conds <- rep(cond_doc, length(docs))
    appearance <- qrel_ptr$getCondApperanceCount(query, conds, docs)
    prefCount <- qrel_ptr$getCondPrefCount(query, conds, docs)
      return(data.frame(doc=docs,utility=prefCount/(appearance+2)))
  }

  util_score <- ddply(data.frame(conditions=conditions), .(conditions),
                      computeScore)

  return(util_score)
}

# for( qid in ){
# 
# 
# x <- subset(getTripletUtility(trels, qid), conditions == "")[order(subset(getTripletUtility(trels, qid), conditions == "")$utility, decreasing = T),]
# 
# 
# tab_out <- data.frame(query=rep(qid,10), QO=rep('Q0',10),docid = x$doc[1:10], rank=seq(1,10), score=x$utility[1:10], runid=rep('ideal'))
# write.table(file = '~/work/data/qrels/ideal.res', append = T, quote = F,row.names = F, col.names = F)
# }

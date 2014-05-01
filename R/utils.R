################################################################################
# TREC Utilities


#' @title Parse TREC Web Track Topics
#'
#' @description
#' \code{eval.adhoc} returns a list of topics, descriptions and subtopics parsed
#' from the topics XML provided by TREC.
#' 
#' @param filename path to the XML topic file.
#'  
#' @details
#' The function is used to parse the web track topics file (XML) provided by
#' TREC. The output consists of a list of topics with topic number, query text,
#' description and subtopics.
#' 
#' @export
parseWebTopicDesc <- function(filename){
  
  doc <- xmlParse(filename)
  topics <- getNodeSet(doc, "//topic")
  process_TREC_topic <- function(t){
    desc <- xmlValue(xmlChildren(t)$description)
    subtopics <- sapply(getNodeSet(t, path="subtopic"), xmlValue)
    
    # Text clean up text
    subtopics <- gsub( "^\\s+|\\s+$|\n|\"|\'|\\,", "",subtopics)
    subtopics <- gsub("\\s{2,}"," ",subtopics)
    
    desc <- gsub( "^\\s+|\\s+$|\n|\"|\'|\\,", "",desc)
    desc <- gsub("\\s{2,}"," ",desc)
    
    return(list(number=xmlGetAttr(t, "number"),
                type=xmlGetAttr(t, "type"),
                text=xmlValue(xmlChildren(t)$query),
                desc=desc,
                subtopics=subtopics))
  }
  topics <- lapply(topics, process_TREC_topic)
  names(topics) <- sapply(topics, function(x) x$number)
  
  return(topics)  
}

################################################################################
# Functions to Sort the System Runs


runSort <- function(runs, qid, runid, sort_type= c('trecSort', 'rankSort')){
  sort_type <- match.arg(sort_type)
  run <- c()
  
  if(is.null(runs$getScoreMatrix(qid)[,runid])) return(NULL)
  
  if(sort_type == 'trecSort'){
    scoreMat <- runs$getScoreMatrix(qid)[,runid]
    scoreMat <- scoreMat[scoreMat > 0] 
    run <- names(scoreMat[order(scoreMat, names(scoreMat),decreasing=T)])
  }else if(sort_type == 'rankSort'){
    rankMat <- runs$getRankMatrix(qid)[,runid]
    run <- names(sort(rankMat[rankMat > 0])) 
  }
  return(run)
}


################################################################################
# Other Utils

resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}


################################################################################
# Summarize Function

summaryzz <- function(){
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
}

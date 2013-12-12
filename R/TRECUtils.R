# TREC Utilities

parseWebTopicDesc <- function(filename){
  
  doc = xmlParse(filename)
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


resave <- function(..., list = character(), file) {
  previous  <- load(file)
  var.names <- c(list, as.character(substitute(list(...)))[-1L])
  for (var in var.names) assign(var, get(var, envir = parent.frame()))
  save(list = unique(c(previous, var.names)), file = file)
}

library(evalIR)

getSTopic_Table <- function(qrels){
  queries <- qrels$getQueries()
  values <- sapply(queries, function(x) length(qrels$getSubtopicProbabilties(x)))
  data.frame(table(values))
}
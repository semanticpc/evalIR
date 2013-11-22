library(evalIR)
qrels <- read.qrels('demo/data/wt2010-adhoc.qrels', type='adhoc')

runs <- read.runs(runPaths=list.files('demo/data/',
                                      pattern='\\.res$',
                                      full.names=T))


eval.adhoc(qrels,runs, ranks=c(20))

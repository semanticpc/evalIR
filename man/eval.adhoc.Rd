\name{eval.adhoc}
\alias{eval.adhoc}
\title{Ad-hoc Evaluation}
\usage{
eval.adhoc(qrels, runs, measures = NULL, decimal = 4, ranks = NULL,
  sort = c("trecSort", "rankSort"))
}
\arguments{
  \item{qrels}{C++ object returned by \code{read.qrels}}

  \item{runs}{C++ object returned by \code{read.runs}}

  \item{measures}{a character vector containing a set of
  evaluation measures.  Accepted measures are c('num_ret',
  'num_rel', 'num_rel_ret', 'recall','AP
  ,'recip_rank','Rprec', 'Prec', 'DCG', 'nDCG', 'ERR',
  'nERR'). Make sure to use the exact same names.}

  \item{ranks}{A numeric vector contisting of a set of
  ranks for which scores are to be computed. Applies only
  to certain measure that needs a cut off like Prec}
}
\description{
\code{eval.adhoc} returns a matrix with the scores returned
by the evaluation measures for each system run per topic.
}
\details{
The function is the main entry to obtain the scores for a
set of evaluation measures. The returns a matrix with the
following headers: runid, queryid, nDCG
}


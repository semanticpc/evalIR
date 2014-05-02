\name{AlphanDCG}
\alias{AlphanDCG}
\title{Alpha - Normalized Discounted Cumulative Gain at k}
\usage{
AlphanDCG(qrels_grades, grades, ranks, idealMatrix = NULL, alpha = 0.5)
}
\arguments{
  \item{qrels_grades}{a numeric matrix containing the
  grades all the judged documents in the qrels file.}

  \item{grades}{a numeric matrix of the grades.}

  \item{ranks}{a numeric vector containing a set of ranks
  at which the measure is calculated.}
}
\description{
\code{AP} Precision
}


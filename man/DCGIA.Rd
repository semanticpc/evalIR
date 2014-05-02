\name{DCGIA}
\alias{DCGIA}
\title{Discounted Cumulative Gain - Intent Aware}
\usage{
DCGIA(qrels_grades, grades, ranks, subtopicProbDist = NULL, alpha = 0.5)
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


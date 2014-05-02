\name{AP}
\alias{AP}
\title{Mean Average Precision}
\usage{
AP(qrels_grades, grades, rankLimit = 1000)
}
\arguments{
  \item{qrels_grades}{a numeric vector containing the
  grades all the judged documents in the qrels file.}

  \item{grades}{a numeric vector of the grades.}

  \item{rankLimit}{document cut off}
}
\description{
\code{AP} Average Precision
}


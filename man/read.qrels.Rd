\name{read.qrels}
\alias{read.qrels}
\title{Read Judgments}
\usage{
read.qrels(qrelsPath, type = c("adhoc", "diversity"))
}
\arguments{
  \item{qrelsPath}{path to the qrels file}

  \item{type}{type of the qrels file. ('adhoc', diversity')
  are currently supported.}
}
\description{
\code{read.qrels} returns a representation of the qrels.
}
\details{
The function creates a C++ object containing various
information parsed from the qrels file.
}


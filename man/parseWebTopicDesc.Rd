\name{parseWebTopicDesc}
\alias{parseWebTopicDesc}
\title{Parse TREC Web Track Topics}
\usage{
parseWebTopicDesc(filename)
}
\arguments{
  \item{filename}{path to the XML topic file.}
}
\description{
\code{eval.adhoc} returns a list of topics, descriptions
and subtopics parsed from the topics XML provided by TREC.
}
\details{
The function is used to parse the web track topics file
(XML) provided by TREC. The output consists of a list of
topics with topic number, query text, description and
subtopics.
}


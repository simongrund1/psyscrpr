\name{findPattern}
\alias{findPattern}
\title{Find pattern in text extracted from PDFs}
\description{
Finds one or several patterns in text extracted from PDFs.
}

\usage{

findPattern(x, pattern, by.page=FALSE, ...)

}

\arguments{

  \item{x}{a data frame as produced by the search methods.}
  \item{pattern}{a character vector denoting patterns to search. Can be literal strings or regular expressions.}
  \item{by.page}{a logical flag indicating of files should be searched by page. Default is \code{FALSE}.}
  \item{...}{additional arguments passed to further function (see Details).}

}

\details{

This function scans the text identified by search methods either on a page-by-page basis of globally in the full text.
The behavior can be fine-tuned by passing additional arguments (see \code{\link{cleanText}}).

}

\value{

Returns a list containing the search results (separated by page if \code{by.page=TRUE}).

}

\author{Simon Grund}
\seealso{\code{\link{scanFiles}}, \code{\link{cleanText}}}

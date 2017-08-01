\name{cleanText}
\alias{cleanText}
\title{Clean text and remove unwanted characters}
\description{
Clean text by removing whitespace, numbers, symbols, and/or conversion to lower case for better searching.
}

\usage{

cleanText(x, strip.whitespace=TRUE, strip.numbers=TRUE, strip.symbols=TRUE,
          lowercase=TRUE)

}

\arguments{

  \item{x}{a character vector.}
  \item{strip.whitespace}{a logical flag indicating if whitespace should be removed. Default is \code{TRUE}.}
  \item{strip.numbers}{a logical flag indicating if numbers should be removed. Default is \code{TRUE}.}
  \item{strip.symbols}{a logical flag indicating if punctuation and non-ASCII characters should be removed. Default is \code{TRUE}.}
  \item{lowercase}{a logical flag indicating if text should be converted to lower case. Default is \code{TRUE}.}

}

\value{

Returns a character string containing the (cleaned) text.

}

\author{Simon Grund}
\seealso{\code{\link{scanFiles}}, \code{\link{findPattern}}}

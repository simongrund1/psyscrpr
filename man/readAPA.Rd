\name{readAPA}
\alias{readAPA}
\title{Read text from PDF (APA)}
\description{
Reads text from publisher PDFs through XML conversion.
}

\usage{

readAPA(file=".", clean=TRUE, reduce=TRUE, relax=FALSE, remove.all.script=TRUE,
        negotiate=TRUE, meta=c("author","year","journal","doi"))

}

\arguments{

  \item{file}{see \code{\link{scanFiles}}.}
  \item{clean}{see \code{\link{scanFiles}}.}
  \item{reduce}{a logical flag indicating if the output is reduced to include no XML data. Default is \code{TRUE}.}
  \item{relax}{see \code{\link{scanFiles}}.}
  \item{remove.all.script}{see \code{\link{scanFiles}}.}
  \item{negotiate}{see \code{\link{scanFiles}}.}
  \item{meta}{see \code{\link{scanFiles}}.}

}

\details{

This function serves as a search method for PDFs formatted with the layout of the American Psychological Association (APA).

}

\value{

Returns a data frame including the text obtained from the PDF (and XML data if \code{reduce=FALSE}).

}

\author{Simon Grund}
\seealso{\code{\link{scanFiles}}}

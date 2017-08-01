\name{scanFiles}
\alias{scanFiles}
\title{Scan PDF files for occurrences of patterns}
\description{
Provides a summary of the occurrence of one or several patterns in one or several PDF files in the publisher-specific layouts in psychological journals.
}

\usage{

scanFiles(file=".", pattern, by.page=FALSE, clean=TRUE, relax=FALSE,
          remove.all.script=TRUE, method=c("APA"), negotiate=TRUE,
          meta=c("author","year","journal","doi"), file.names=FALSE, ...)

}

\arguments{

  \item{file}{a character vector denoting file names or (full) paths. Default is the current working directory.}
  \item{pattern}{a character vector denoting patterns to search. Can be literal strings or regular expressions.}
  \item{by.page}{a logical flag indicating of files should be searched by page. Default is \code{FALSE}.}
  \item{clean}{a logical flag indicating if text should be cleaned to search only the main text. Default is \code{TRUE}.}
  \item{relax}{a logical or integer value indicating tolerance in the horizontal orientation for identifying main text. Default is \code{FALSE}.}
  \item{remove.all.script}{a logical flag indicating if script sized text should be removed before searching (e.g., footnotes, table notes). Default is \code{TRUE}.}
  \item{method}{a character string denoting the document type. Currently, only \code{"APA"} is supported.}
  \item{negotiate}{a logical flag indicating if meta data should be collected with DOI content negotiation. Default is \code{TRUE}.}
  \item{meta}{a character vector denoting what meta data to extract.}
  \item{file.names}{a logical flag indicating if file names should be saved. Default is \code{TRUE}.}
  \item{...}{additional arguments passed to further function (see Details).}

}

\details{

This function serves as a wrapper for searching larger collections of files with a common layout.
Each file can be searched either by collapsing the text across pages (more exhaustive) or by searching pages separately (more informative).
By default, the search methods attempt to identify the main text (\code{clean=TRUE}) while ignoring structural components of the articles (e.g., headers, footers, abstract, references).
In addition, searches are confined to strictly regular text by default thus removing script sized passages (e.g., footnotes, table notes).
The behavior can be fine-tuned by passing additional arguments (see also \code{\link{findPattern}} and \code{\link{cleanText}}).

The search methods attempt to identify the main text in the article, among other things, through the horizontal position of text on the page.
By default, the search methods use strict rules for identifying text.
However, because the layout across files can slightly vary even in publisher-formatted PDFs, it can be useful to increase the tolerance with which that is done (e.g., \code{relax=TRUE}).

Currently, the only available search method is \code{"APA"} (two-column layout used by the American Psychological Association).
Further methods may be included in the future.

The function attempts to identify meta information in the article by using the DOI.
Specifically, if a DOI is found, the function attempts to retrieve the meta information from \code{doi.org} through DOI content negotiation.

}

\value{

Returns a data frame with an additional class attribute \code{scrpr.frame} including meta information (if available), file info, and search results.
The content of the data frame depends on whether scans are carried out by page (\code{by.page}) what meta information is requested (\code{meta}).

}

\author{Simon Grund}
\seealso{\code{\link{cleanText}}, \code{\link{findPattern}}}

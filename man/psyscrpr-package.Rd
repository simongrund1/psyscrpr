\name{psyscrpr-package}
\alias{psyscrpr-package}
\docType{package}
\title{mitml: Tools for scraping text from PDFs of psychological articles}
\description{Provides tools for scraping and searching text from PDFs of psychological in publisher-specific layouts.

This package provides tools for processing text from psychological articles in PDF format.
It requires the \code{pdftohtml} program to be installed locally (from the Poppler library, http://poppler.freedesktop.org).

To process text within files, \code{psyscrpr} converts files to XML format in order to incorporate information about position and format of text.
Furthermore, the package attempts to clean text, for example, by removing tables, figures, etc. as well as by stripping whitespace, numbers, and symbols.
Search patterns may include regular expressions or conventional strings.

\bold{Note:} This package should be regarded as a screening tool.
By their nature, the methods for the analysis of PDF files implemented in this package are not exact, and the use of specialized APIs should be preferred if high accuracy is required.

}
\author{
Authors: Simon Grund

Maintainer: Simon Grund <grund@ipn.uni-kiel.de>
}
\keyword{package}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipelinederivative_outputs.R
\name{getsrtmresults_skiprows}
\alias{getsrtmresults_skiprows}
\title{Determine the number of rows to skip}
\usage{
getsrtmresults_skiprows(filename)
}
\arguments{
\item{filename}{Filename of the text output from kinfitresults}
}
\value{
The number of rows to skip for the outcome parameters
}
\description{
Convenience function for getsrtmresults
}
\examples{
getsrtmresults_skiprows('abcd_1_kinfitresults.txt')

}

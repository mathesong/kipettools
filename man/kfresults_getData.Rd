% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline_outputs.R
\name{kfresults_getData}
\alias{kfresults_getData}
\title{Extract TAC data from kinfitresults}
\usage{
kfresults_getData(matfile, use_rmatio = FALSE, use_SparseM = FALSE)
}
\arguments{
\item{matfile}{Filename of the kinfitresults file}

\item{use_rmatio}{Should rmatio be used for extraction? It tends to be faster, but sometimes fails. But sometimes only rmatio works. I have no idea why...}

\item{use_SparseM}{Try setting this to true if things aren't working with rmatio or without. Use without rmatio.}
}
\value{
List with the sizes of all rois, the subject name, PET number, and
TAC data
}
\description{
Extract TAC data from a kinfitresults.mat file
}
\examples{
abcd_out <- kfresults_getData('abcd_1_kinfitresults.mat')

}

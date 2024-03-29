% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipelinederivative_outputs.R
\name{srtm2xlsx}
\alias{srtm2xlsx}
\title{Extract data from srtmresults files into a single spreadsheet.}
\usage{
srtm2xlsx(
  studyflag = F,
  filesflag = NULL,
  filepattern = "*_kinfitresults.mat.txt"
)
}
\arguments{
\item{studyflag}{Logical: is there a study name flag on each filename (e.g.
Study1_abcd_kinfitresults)}

\item{filesflag}{Character: should the output be saved with a special
filename flag. Default is NULL.}

\item{filepattern}{Character: pattern to identify the srtmresults txt files.}
}
\value{
Creates an xlsx file with the outcome parameters for each subject for
all subjects in a directory.
}
\description{
Extract outcome parameters from a folder of srtmresults files.  We could use
the raw mat files, but they take ages to load into R.  It is therefore better
to run the SRTMresults scripts in MATLAB to generate the text files first,
and then to get the data out of the text files.
}
\examples{
srtm2xlsx()

}

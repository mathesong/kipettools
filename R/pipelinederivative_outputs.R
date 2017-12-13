#' Extract outcome parameters from an srtmresults text file
#'
#' Convenience function for srtm2xlsx.  Extract the outcome parameters from a
#' srtmresults file.
#'
#' @param filename Filename of the txt file
#' @param skipno The number of rows to skip (from getsrtmresults_skiprows)
#'
#' @return The outcome parameters
#' @export
#'
#' @examples
#' getsrtmresults('abcd_1_kinfitresults.txt', 15)
#'
getsrtmresults <- function(filename, skipno) {


  # Reading in the file

  skipno <- getsrtmresults_skiprows(filename)

  a <- read.table(file = filename, skip = skipno, header = TRUE, sep = " ", comment.char = "?")


  # Check that skiprows is correct (this takes time, therefore better not to use it)

  if (colnames(a)[1] != "region") {
    a <- read.table(file = filename, skip = getsrtmresults_skiprows(), header = TRUE, comment.char = "?")
  }

  Regions <- a$region
  Regions <- gsub(pattern = "\t", replacement = "", x = Regions)

  BP <- a$BP_nd
  R <- a$R
  k2 <- a$k2

  subjname_end <- unlist(gregexpr("_kinfitresults.mat.txt", text = filename))
  SubjName <- substr(x = filename, start = 1, stop = subjname_end - 3)
  PETNo <- substr(x = filename, start = subjname_end - 1, stop = subjname_end - 1)

  p <- c(SubjName, BP)

  output <- list(SubjName, PETNo, Regions, R, k2, BP)

  names(output) <- c("Subject", "PETNo", "Regions", "R", "k2", "BP")

  return(output)
}


#' Determine the number of rows to skip
#'
#' Convenience function for getsrtmresults
#'
#' @param filename Filename of the text output from kinfitresults
#'
#' @return The number of rows to skip for the outcome parameters
#' @export
#'
#' @examples
#' getsrtmresults_skiprows('abcd_1_kinfitresults.txt')
#'
getsrtmresults_skiprows <- function(filename) {

  # Determining how many rows to skip

  b <- read.csv(filename, comment.char = "?")

  startrow <- vector()

  for (i in 1:nrow(b)) {
    startrow[i] <- as.character(b[i, ]) == "SRTM estimated parameters"
  }

  skipno <- which(startrow) + 3

  return(skipno)
}


#' Extract data from srtmresults files into a single spreadsheet.
#'
#' Extract outcome parameters from a folder of srtmresults files.  We could use
#' the raw mat files, but they take ages to load into R.  It is therefore better
#' to run the SRTMresults scripts in MATLAB to generate the text files first,
#' and then to get the data out of the text files.
#'
#' @param studyflag Logical: is there a study name flag on each filename (e.g.
#'   Study1_abcd_kinfitresults)
#' @param filesflag Character: should the output be saved with a special
#'   filename flag. Default is NULL.
#' @param filepattern Character: pattern to identify the srtmresults txt files.
#'
#' @return Creates an xlsx file with the outcome parameters for each subject for
#'   all subjects in a directory.
#' @export
#'
#' @examples
#' srtm2xlsx()
#'
srtm2xlsx <- function(studyflag = F, filesflag=NULL, filepattern = "*_kinfitresults.mat.txt") {
  files <- list.files(pattern = filepattern)

  if (!is.null(filesflag)) {
    files <- files[grepl(filesflag, files)]
  }

  skipno <- getsrtmresults_skiprows(filename = files[1])


  ##### Get Regions

  # Define unique regions for whole group

  Regions <- vector()

  for (j in 1:length(files)) {
    Region <- getsrtmresults(filename = files[j], skipno)$Region
    Regions <- c(Regions, Region)
  }
  Regions <- unique(Regions)


  # Sorting Regions

  LeftRightRegions <- vector(length = length(Regions))
  for (n in 1:length(Regions)) {
    if (substr(Regions[n], start = 1, stop = 2) == "L_") {
      LeftRightRegions[n] <- 2
    } else if (substr(Regions[n], start = 1, stop = 2) == "R_") {
      LeftRightRegions[n] <- 3
    } else {
      LeftRightRegions[n] <- 1
    }
  }

  Regions <- Regions[order(LeftRightRegions)]



  BPSheet <- matrix(ncol = length(Regions), nrow = length(files))

  colnames(BPSheet) <- c(Regions)



  Subjnames <- vector()
  PETNo <- vector()
  StudyName <- vector()

  for (i in 1:length(files)) {
    data <- getsrtmresults(filename = files[i], skipno)

    # BPSheet[i,1] <- data$Subject

    if (studyflag == T) {
      splitname <- stringr::str_split(data$Subject, pattern = "_")
      Subjnames[i] <- splitname[[1]][2]
      StudyName[i] <- splitname[[1]][1]
    } else {
      Subjnames[i] <- data$Subject
      StudyName[i] <- NA
    }

    PETNo[i] <- as.numeric(data$PETNo)

    for (k in 1:length(data$Regions)) {
      colnum <- which(Regions == data$Region[k])
      BPSheet[i, colnum] <- as.numeric(data$BP[k])
    }
  }

  if (studyflag == F) {
    BP_Data <- data.frame(Subjnames, PETNo, BPSheet)
  } else {
    BP_Data <- data.frame(Subjnames, StudyName, PETNo, BPSheet)
  }


  xlsx::write.xlsx(x = BP_Data, file = "BP_Sheet.xlsx", row.names = FALSE)
}

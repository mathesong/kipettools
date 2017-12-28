#' Extract TAC data from kinfitresults
#'
#' Extract TAC data from a kinfitresults.mat file
#'
#' @param matfile Filename of the kinfitresults file
#'
#' @return List with the sizes of all rois, the subject name, PET number, and
#'   TAC data
#' @export
#'
#' @examples
#' abcd_out <- kfresults_getData('abcd_1_kinfitresults.mat')
#'
kfresults_getData <- function(matfile) {

  # Checks

  n_underscores <- stringr::str_count(matfile, "_")

  if (n_underscores == 2) {
    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 3)
    Subjname <- details[, 1]
    PETNo <- as.numeric(details[, 2])
  }

  if (n_underscores == 3) {
    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 4)
    Subjname <- paste(details[, 1], details[, 2], sep = "_")
    PETNo <- as.numeric(details[, 3])
  }

  if (n_underscores != 2 & n_underscores != 3) {
    stop("\nThere should be either two or three underscores in the filename.
         Either this file is incorrectly named with its pipeline output,
         or it is not a kinfitresults file.")
  }


  # Doing the deeds

  kfresults <- R.matlab::readMat(matfile)

  times <- as.numeric(kfresults$data[, , 1]$troi) / 60
  gm_tacdata <- as.data.frame(kfresults$data[, , 1]$stats[, , 1]$greymasked[, , 1]$mean)
  raw_tacdata <- as.data.frame(kfresults$data[, , 1]$stats[, , 1]$raw[, , 1]$mean)

  tacdata <- raw_tacdata

  roinames <- unlist(kfresults$data[, , 1]$stats[, , 1]$roinames)
  roinames <- gsub("#", "gm", roinames)

  gmrois <- which(grepl(pattern = "gm", roinames))

  tacdata[, gmrois] <- gm_tacdata[, gmrois]

  names(tacdata) <- roinames

  tacdata <- rbind(0, tacdata)

  tacdata$times <- times
  tacdata$weights <- unlist(kfresults$data[, , 1]$weights[, 1])

  gm_roisizes <- kfresults$data[, , 1]$stats[, , 1]$greymasked[, , 1]$vol[1, ]
  raw_roisizes <- kfresults$data[, , 1]$stats[, , 1]$raw[, , 1]$vol[1, ]

  roisizes <- raw_roisizes
  roisizes[gmrois] <- gm_roisizes[gmrois]
  roisizes <- data.frame(ROI = roinames, Volume = roisizes)


  out <- list(roisizes = roisizes, tacdata = tacdata, Subjname = Subjname, PETNo = PETNo)

  return(out)
}




#' Extract TAC data from roistats
#'
#' Extract TAC data from a kinfitresults.mat file
#'
#' @param matfile Filename of the roistats file
#'
#' @return List with the sizes of all rois, the subject name, PET number, and
#'   TAC data
#' @export
#'
#' @examples
#' abcd_out <- roistats_getData('abcd_dynpet1_roistats.mat')
#'
roistats_getData <- function(matfile) {

  # Checks

  n_underscores <- stringr::str_count(matfile, "_")

  if (n_underscores == 2) {
    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 2)
    Subjname <- details[, 1]
    PETNo <- as.numeric(stringr::str_match(matfile, ".*dynpet(\\d+).*")[2])
  }

  if (n_underscores == 3) {
    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 3)
    Subjname <- paste(details[, 1], details[, 2], sep = "_")
    PETNo <- as.numeric(stringr::str_match(matfile, ".*dynpet(\\d+).*")[2])
  }

  if (n_underscores != 2 & n_underscores != 3) {
    stop("\nThere should be either two or three underscores in the filename.
         Either this file is incorrectly named with its pipeline output,
         or it is not a roistats file.")
  }


  # Doing the deeds


  roistats <- R.matlab::readMat(matfile)

  times <- as.numeric(roistats$stats[, , 1]$times[, , 1]$center)
  gm_tacdata <- as.data.frame(unlist(roistats$stats[, , 1]$greymasked[3][[1]]))
  raw_tacdata <- as.data.frame(unlist(roistats$stats[, , 1]$raw[3][[1]]))

  roinames <- unlist(roistats$stats[, , 1]$roinames)
  roinames <- gsub("#", "gm", roinames)
  gmrois <- which(grepl(pattern = "gm", roinames))

  tacdata <- raw_tacdata
  tacdata[, gmrois] <- gm_tacdata[, gmrois]
  names(tacdata) <- roinames
  tacdata <- rbind(0, tacdata)

  tacdata <- cbind(c(0, times), tacdata)
  names(tacdata)[1] <- "Times"

  gm_roisizes <- roistats$stats[, , 1]$greymasked[, , 1]$vol[1, ]
  raw_roisizes <- roistats$stats[, , 1]$raw[, , 1]$vol[1, ]

  roisizes <- raw_roisizes
  roisizes[gmrois] <- gm_roisizes[gmrois]

  details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 2)
  Subjname <- details[, 1]
  PETNo <- as.numeric(stringr::str_match(matfile, ".*dynpet(\\d+).*")[2])

  out <- list(roisizes = roisizes, tacdata = tacdata, Subjname = Subjname, PETNo = PETNo)

  return(out)
}

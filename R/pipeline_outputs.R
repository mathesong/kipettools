#' Extract TAC data from kinfitresults
#'
#' Extract TAC data from a kinfitresults.mat file
#'
#' @param matfile Filename of the kinfitresults file
#' @param use_rmatio Should rmatio be used for extraction? It tends to be faster, but sometimes fails. But sometimes only rmatio works. I have no idea why...
#' @param use_SparseM Try setting this to true if things aren't working with rmatio or without. Use without rmatio.
#'
#' @return List with the sizes of all rois, the subject name, PET number, and
#'   TAC data
#' @export
#'
#' @examples
#' abcd_out <- kfresults_getData('abcd_1_kinfitresults.mat')
#'
kfresults_getData <- function(matfile, use_rmatio = FALSE, use_SparseM = FALSE) {

  # Checks

  filename <- basename(matfile)

  n_underscores <- stringr::str_count(filename, "_")

  if (n_underscores == 2) {
    details <- stringr::str_split_fixed(filename, pattern = "_", n = 3)
    Subjname <- details[, 1]
    PETNo <- as.numeric(details[, 2])
  }

  if (n_underscores == 3) {
    details <- stringr::str_split_fixed(filename, pattern = "_", n = 4)
    Subjname <- paste(details[, 1], details[, 2], sep = "_")
    PETNo <- as.numeric(details[, 3])
  }

  if (n_underscores != 2 & n_underscores != 3) {
    stop("\nThere should be either two or three underscores in the filename.
         Either this file is incorrectly named with its pipeline output,
         or it is not a kinfitresults file.")
  }


  # Doing the deeds

  if(use_rmatio) {

    kfresults <- rmatio::read.mat(matfile)
    # kfresults <- R.matlab::readMat(matfile)

    times <- as.numeric(kfresults$data$troi[[1]]) / 60
    durations <- c(0, as.numeric(kfresults$data$stats$times$duration[[1]]) / 60)

    gm_tacdata <- as.data.frame(kfresults$data$stats$greymasked$mean[[1]])
    raw_tacdata <- as.data.frame(kfresults$data$stats$raw$mean[[1]])

    tacdata <- raw_tacdata

    roinames <- unlist(kfresults$data$stats$roinames[[1]])
    roinames <- gsub("#", "gm", roinames)

    gmrois <- which(grepl(pattern = "gm", roinames))

    tacdata[, gmrois] <- gm_tacdata[, gmrois]

    names(tacdata) <- roinames

    tacdata <- rbind(0, tacdata)

    tacdata$times <- times
    tacdata$durations <- durations
    tacdata$weights <- unlist(kfresults$data$weights[[1]])

    gm_roisizes <- kfresults$data$stats$greymasked$vol[[1]]
    raw_roisizes <- kfresults$data$stats$raw$vol[[1]]

    roisizes <- raw_roisizes
    roisizes[,gmrois] <- gm_roisizes[,gmrois]
    roisizes <- data.frame(ROI = roinames, Volume = roisizes[1,])

  } else {

    if(use_SparseM) {

      kfresults <- R.matlab::readMat(matfile, sparseMatrixClass="SparseM")
    } else {
      kfresults <- R.matlab::readMat(matfile)
    }


    times <- as.numeric(kfresults$data[, , 1]$troi) / 60
    durations <- c(0, as.numeric(kfresults$data[, , 1]$stats[, , 1]$times[, , 1]$duration) / 60)

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
    tacdata$durations <- durations
    tacdata$weights <- unlist(kfresults$data[, , 1]$weights[, 1])

    gm_roisizes <- kfresults$data[, , 1]$stats[, , 1]$greymasked[, , 1]$vol[1, ]
    raw_roisizes <- kfresults$data[, , 1]$stats[, , 1]$raw[, , 1]$vol[1, ]

    roisizes <- raw_roisizes
    roisizes[gmrois] <- gm_roisizes[gmrois]
    roisizes <- data.frame(ROI = roinames, Volume = roisizes)

  }




  out <- list(roisizes = roisizes, tacdata = tacdata, Subjname = Subjname, PETNo = PETNo)

  return(out)
}




#' Extract TAC data from roistats
#'
#' Extract TAC data from a kinfitresults.mat file
#'
#' @param matfile Filename of the roistats file
#' @param use_rmatio Should rmatio be used for extraction? It tends to be faster, but sometimes fails. But sometimes only rmatio works. I have no idea why...
#'
#' @return List with the sizes of all rois, the subject name, PET number, and
#'   TAC data
#' @export
#'
#' @examples
#' abcd_out <- roistats_getData('abcd_dynpet1_roistats.mat')
#'
roistats_getData <- function(matfile, use_rmatio = FALSE) {

  # Checks

  filename <- basename(matfile)

  n_underscores <- stringr::str_count(filename, "_")

  if (n_underscores == 2) {
    details <- stringr::str_split_fixed(filename, pattern = "_", n = 2)
    Subjname <- details[, 1]
    PETNo <- as.numeric(stringr::str_match(filename, ".*pet(\\d+).*")[2])
  }

  if (n_underscores == 3) {

    details <- stringr::str_split_fixed(filename, pattern = "_", n = 3)

    if(stringr::str_detect(filename, "realigned_roistats")) {
      Subjname <- details[, 1]
      PETNo <- as.numeric(stringr::str_match(filename, ".*pet(\\d+).*")[2])
    } else {
      Subjname <- paste(details[, 1], details[, 2], sep = "_")
      PETNo <- as.numeric(stringr::str_match(filename, ".*pet(\\d+).*")[2])
    }
  }

  if (n_underscores != 2 & n_underscores != 3) {
    stop("\nThere should be either two or three underscores in the filename.
         Either this file is incorrectly named with its pipeline output,
         or it is not a roistats file.")
  }


  # Doing the deeds

  if(use_rmatio) {

    roistats <- rmatio::read.mat(matfile)
    # roistats <- R.matlab::readMat(matfile)

    times <- c(0, as.numeric(roistats$stats$times$center[[1]])) / 60
    durations <- c(0, as.numeric(roistats$stats$times$duration[[1]]))/60

    gm_tacdata <- as.data.frame(roistats$stats$greymasked$mean[[1]])
    raw_tacdata <- as.data.frame(roistats$stats$raw$mean[[1]])

    roinames <- unlist(roistats$stats$roinames[[1]])
    roinames <- gsub("#", "gm", roinames)
    gmrois <- which(grepl(pattern = "gm", roinames))

    tacdata <- raw_tacdata
    tacdata[, gmrois] <- gm_tacdata[, gmrois]
    names(tacdata) <- roinames
    tacdata <- rbind(0, tacdata)

    tacdata$times <-times
    tacdata$durations <- durations
    #names(tacdata)[1] <- "Times"

    gm_roisizes <- roistats$stats$greymasked$vol[[1]]
    raw_roisizes <- roistats$stats$raw$vol[[1]]

    roisizes <- raw_roisizes
    roisizes[gmrois] <- gm_roisizes[gmrois]
    roisizes <- data.frame(ROI = roinames, Volume = roisizes[1,])

    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 2)
    Subjname <- details[, 1]
    PETNo <- as.numeric(stringr::str_match(matfile, ".*dynpet(\\d+).*")[2])

  } else {

    roistats <- R.matlab::readMat(matfile)

    times <- c(0, as.numeric(roistats$stats[, , 1]$times[, , 1]$center)) / 60
    durations <- c(0, as.numeric(roistats$stats[, , 1]$times[, , 1]$duration))/60

    gm_tacdata <- as.data.frame(unlist(roistats$stats[, , 1]$greymasked[3][[1]]))
    raw_tacdata <- as.data.frame(unlist(roistats$stats[, , 1]$raw[3][[1]]))

    roinames <- unlist(roistats$stats[, , 1]$roinames)
    roinames <- gsub("#", "gm", roinames)
    gmrois <- which(grepl(pattern = "gm", roinames))

    tacdata <- raw_tacdata
    tacdata[, gmrois] <- gm_tacdata[, gmrois]
    names(tacdata) <- roinames
    tacdata <- rbind(0, tacdata)

    tacdata$times <-times
    tacdata$durations <- durations
    #names(tacdata)[1] <- "Times"

    gm_roisizes <- roistats$stats[, , 1]$greymasked[, , 1]$vol[1, ]
    raw_roisizes <- roistats$stats[, , 1]$raw[, , 1]$vol[1, ]

    roisizes <- raw_roisizes
    roisizes[gmrois] <- gm_roisizes[gmrois]
    roisizes <- data.frame(ROI = roinames, Volume = roisizes[1,])

    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 2)
    Subjname <- details[, 1]
    PETNo <- as.numeric(stringr::str_match(matfile, ".*dynpet(\\d+).*")[2])

  }

  out <- list(roisizes = roisizes, tacdata = tacdata, Subjname = Subjname, PETNo = PETNo)

  return(out)
}


#' Creates a BIDS list from a pipeline anc.mat file
#'
#' This function creates a BIDS list from a pipeline anc file.
#'
#' @param matfile The anc.mat file generated by the pipeline.
#'
#' @return A list in BIDS structure
#' @export
#'
#' @examples
#' \dontrun{
#' ancmat_getData('ancfile.mat')
#' }
ancmat_getData <- function(matfile) {

  # Checks

  filename <- basename(matfile)

  n_underscores <- stringr::str_count(filename, "_")

  if (n_underscores == 2) {
    details <- stringr::str_split_fixed(filename, pattern = "_", n = 3)
    Subjname <- details[, 1]
    PETNo <- as.numeric(details[, 2])
  }

  if (n_underscores == 3) {
    details <- stringr::str_split_fixed(basename(matfile), pattern = "_", n = 4)

    if(details[,3] == "arterial") {
      Subjname <- details[, 1]
      PETNo <- as.numeric(details[, 2])
    } else {
      Subjname <- details[, 2]
      PETNo <- as.numeric(details[, 3])
      }
    }

  if (n_underscores != 2 & n_underscores != 3) {
    stop("\nThere should be either two or three underscores in the filename.
         Either this file is incorrectly named with its pipeline output,
         or it is not an ancmat file.")
  }


  # Doing the deeds

  ancmat <- R.matlab::readMat(matfile)

  Info <- list(
    Tomograph = ancmat$Info[, , 1]$tomograph[1],
    Anaesthesia = ancmat$Info[, , 1]$anaesthesia[1],
    BodyPart = ancmat$Info[, , 1]$bodyPart[1],
    #Unit = ,
    Tracer = list(
      Name = ancmat$Info[, , 1]$Tracer[, , 1]$name[1],
      Isotope = ancmat$Info[, , 1]$Tracer[, , 1]$isotope[1],
      MW = ancmat$Info[, , 1]$Tracer[, , 1]$MW[1],
      InjectionType = ancmat$Info[, , 1]$Tracer[, , 1]$injectionType[1]
      ),
    Pharmaceutical = list(
      Name = ancmat$Info[, , 1]$Pharmaceutical[,,1]$name[1],
      DoseAmount = ancmat$Info[, , 1]$Pharmaceutical[,,1]$doseAmount[1],
      DoseUnits = ancmat$Info[, , 1]$Pharmaceutical[,,1]$doseUnits[1],
      DoseRegimen = ancmat$Info[, , 1]$Pharmaceutical[,,1]$doseRegimen[1],
      DoseTime = ancmat$Info[, , 1]$Pharmaceutical[,,1]$doseTime[1],
      DoseTimeUnits = ancmat$Info[, , 1]$Pharmaceutical[,,1]$doseTimeUnits[1]
      )
    )

  Radiochem = list(
    InjectedRadioactivity = ancmat$Radiochem[,,1]$injectedRadioactivity[1],
    InjectedRadioactivityUnits = ancmat$Radiochem[,,1]$injectedRadioactivityUnits[1],
    InjectedMass = ancmat$Radiochem[,,1]$injectedMass[1],
    InjectedMassUnits = ancmat$Radiochem[,,1]$injectedMassUnits[1],
    SpecificRadioactivity = ancmat$Radiochem[,,1]$specificRadioactivity[1],
    SpecificRadioactivityUnits = ancmat$Radiochem[,,1]$specificRadioactivityUnits[1],
    #SpecificRadioactivityMeasTime = ancmat$Radiochem[,,1]$specificRadioactivity[1],
    MinPurity = ancmat$Radiochem[,,1]$minPurity[1],
    MinPurityUnits = ancmat$Radiochem[,,1]$minPurityUnits[1]
  )

  Time = list(
    ScanStart = ancmat$Time[,,1]$scanStart[1],
    ScanStartUnits = ancmat$Time[,,1]$scanStartUnits[1],
    InjectionStart = ancmat$Time[,,1]$injectionStart[1],
    InjectionStartUnits = ancmat$Time[,,1]$injectionStartUnits[1],
    InjectionEnd = ancmat$Time[,,1]$injectionEnd[1],
    InjectionEndUnits = ancmat$Time[,,1]$injectionEndUnits[1],
    FrameTime = list(
      Labels = unlist(ancmat$Time[,,1]$FrameTimes[,,1]$labels),
      Units = unlist(ancmat$Time[,,1]$FrameTimes[,,1]$units),
      Values = unlist(ancmat$Time[,,1]$FrameTimes[,,1]$values)
    )
  )

  Plasma = list(
    FreeFraction = ancmat$Plasma[,,1]$freefraction[1],
    Data = list(
      # FreeFractionMethod = ancmat$Plasma[,,1]$freefraction[1],
      # FreeFractionTime = ,
      DecayCorrected = ancmat$Plasma[,,1]$Data[,,1]$decayCorrected[1],
      DecayCorrectionTime = ancmat$Plasma[,,1]$Data[,,1]$decayCorrectionTime[1],
      Type = ancmat$Plasma[,,1]$Data[,,1]$type[1],
      Labels = unlist(ancmat$Plasma[,,1]$Data[,,1]$labels),
      Units = unlist(ancmat$Plasma[,,1]$Data[,,1]$units),
      Values = unlist(ancmat$Plasma[,,1]$Data[,,1]$values)
    )
  )

  Metabolite = list(
    Data = list(
      Type = ancmat$Metabolite[,,1]$Data[,,1]$type[1],
      # Method = ancmat$Metabolite[,,1]$Data[1],
      Labels = unlist(ancmat$Metabolite[,,1]$Data[,,1]$labels),
      Units = unlist(ancmat$Metabolite[,,1]$Data[,,1]$units),
      Values = unlist(ancmat$Metabolite[,,1]$Data[,,1]$values)
    )
  )

  Blood_Discrete = list(
    Haematocrit = ancmat$Blood[,,1]$Discrete[,,1]$haematocrit[1],
    BloodDensity = ancmat$Blood[,,1]$Discrete[,,1]$bloodDensity[1],
    BloodDensityUnits = ancmat$Blood[,,1]$Discrete[,,1]$bloodDensityUnits[1],
    Data = list(
      Type = ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$type[1],
      # Method = ancmat$Blood[,,1]$Discrete[,,1]$Data[1],
      Labels = unlist(ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$labels),
      Units = unlist(ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$units),
      Values = unlist(ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$values),
      DecayCorrected = ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$decayCorrected[1],
      DecayCorrectionTime = ancmat$Blood[,,1]$Discrete[,,1]$Data[,,1]$decayCorrectionTime[1]
    )
  )

  Blood_Continuous = list(
    WithdrawalRate = ancmat$Blood[,,1]$Continuous[,,1]$withdrawalRate[1],
    WithdrawalRateUnits = ancmat$Blood[,,1]$Continuous[,,1]$withdrawalRateUnits[1],
    TubingType = ancmat$Blood[,,1]$Continuous[,,1]$tubingType[1],
    TubingLength = ancmat$Blood[,,1]$Continuous[,,1]$tubingLength[1],
    DispersionConstant = ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$dispersionFactor[1],
    DispersionConstantUnits = ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$dispersionFactorUnits[1],
    DispersionCorrected = TRUE,
    Data = list(
      DecayCorrected = ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$decayCorrected[1],
      DecayCorrectionTime = ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$decayCorrectionTime[1],
      Type = ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$type[1],
      Labels = unlist(ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$labels),
      Units = unlist(ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$units),
      Values = unlist(ancmat$Blood[,,1]$Continuous[,,1]$Data[,,1]$values)
    )
  )

  Blood <- list(
    Discrete = Blood_Discrete,
    Continuous = Blood_Continuous
  )

  bidsout <- list(
    Info = Info,
    Radiochem = Radiochem,
    Time = Time,
    Plasma = Plasma,
    Metabolite = Metabolite,
    Blood = Blood
  )

  # Fixing units

  ## Plasma

  if( bidsout$Plasma$Data$Units[1] == "min" ) {
    bidsout$Plasma$Data$Units[1] = "s"
    bidsout$Plasma$Data$Values[,1] =
      bidsout$Plasma$Data$Values[,1] / 60
  }

  if( bidsout$Plasma$Data$Units[2] == "min" ) {
    bidsout$Plasma$Data$Units[2] = "s"
    bidsout$Plasma$Data$Values[,2] =
      bidsout$Plasma$Data$Values[,2] / 60
  }

  if( bidsout$Plasma$Data$Units[3] == "nCi/ml" ) {
    bidsout$Plasma$Data$Units[3] = "kBq/ml"
    bidsout$Plasma$Data$Values[,3] =
      bidsout$Plasma$Data$Values[,3] * 0.037
  }

  ## Discrete Blood

  if( bidsout$Blood$Discrete$Data$Units[1] == "min" ) {
    bidsout$Blood$Discrete$Data$Units[1] = "s"
    bidsout$Blood$Discrete$Data$Values[,1] =
      bidsout$Blood$Discrete$Data$Values[,1] / 60
  }

  if( bidsout$Blood$Discrete$Data$Units[2] == "min" ) {
    bidsout$Blood$Discrete$Data$Units[2] = "s"
    bidsout$Blood$Discrete$Data$Values[,2] =
      bidsout$Blood$Discrete$Data$Values[,2] / 60
  }

  if( bidsout$Blood$Discrete$Data$Units[3] == "nCi/ml" ) {
    bidsout$Blood$Discrete$Data$Units[3] = "kBq/ml"
    bidsout$Blood$Discrete$Data$Values[,3] =
      bidsout$Blood$Discrete$Data$Values[,3] * 0.037
  }

  ## Continuous Blood

  if( bidsout$Blood$Continuous$Data$Units[1] == "min" ) {
    bidsout$Blood$Continuous$Data$Units[1] = "s"
    bidsout$Blood$Continuous$Data$Values[,1] =
      bidsout$Blood$Continuous$Data$Values[,1] / 60
  }

  if( bidsout$Blood$Continuous$Data$Units[2] == "nCi/ml" ) {
    bidsout$Blood$Continuous$Data$Units[2] = "kBq/ml"
    bidsout$Blood$Continuous$Data$Values[,2] =
      bidsout$Blood$Continuous$Data$Values[,2] * 0.037
  }

  ## Metabolite

  if("units" %in% names(bidsout$Metabolite$Data)) {
    bidsout$Metabolite$Data$Units <- bidsout$Metabolite$Data$units
  }

  if( bidsout$Metabolite$Data$Units[1] == "min" ) {
    bidsout$Blood$Continuous$Data$Units[1] = "s"
    bidsout$Blood$Continuous$Data$Values[,1] =
      bidsout$Blood$Continuous$Data$Values[,1] / 60
  }

  if( bidsout$Metabolite$Data$Units[1] == "min" ) {
    bidsout$Blood$Continuous$Data$Units[1] = "s"
    bidsout$Blood$Continuous$Data$Values[,1] =
      bidsout$Blood$Continuous$Data$Values[,1] / 60
  }

  if( nrow(bidsout$Metabolite$Data$Values) == 0 ) {
    bidsout$Metabolite$Data$Values <- bidsout$Plasma$Data$Values
    bidsout$Metabolite$Data$Values[,3] <- rep(1,
                      nrow(bidsout$Metabolite$Data$Values))
  }

  ## Time Warnings

  plasmatime_max <- max(bidsout$Plasma$Data$Values[,1])
  bloodtime_max <- max(bidsout$Blood$Discrete$Data$Values[,1])
  bloodctime_max <- max(bidsout$Blood$Continuous$Data$Values[,1])
  metabtime_max <- max(bidsout$Metabolite$Data$Values[,1])

  if(! ( plasmatime_max > 30*60 & plasmatime_max < 180*60 )) {
    warning("Plasma times do not look like seconds")
  }

  if(! ( bloodtime_max > 30*60 & bloodtime_max < 180*60 )) {
    warning("Discrete blood times do not look like seconds")
  }

  if(! ( bloodctime_max > 1*60 & bloodctime_max < 15*60 )) {
    warning("Continuous blood times do not look like seconds")
  }

  if(! ( metabtime_max > 20*60 & metabtime_max < 180*60 )) {
    warning("Metabolite times do not look like seconds")
  }

  # if(ancmat$Blood[,,1]$Continuous[,,1]$extraInfo[,,1]$system[,1] == "ALLOGG") {
  #   warning("dispersion constant assumed equal to 2.6 because ALLOGG system used")
  #   dispersionconstant = 2.6
  # } else {
  #   warning("dispersion constant unknown. Set to 2.6")
  #   dispersionconstant = 2.6
  # }


  if(Blood_Continuous$DispersionConstantUnits == "1/min") {
    Blood_Continuous$DispersionConstantUnits == "s"
    Blood_Continuous$DispersionConstant = 60*(1/Blood_Continuous$DispersionConstant)
  }


  blooddata <- kinfitr::create_blooddata_components(
    Blood.Discrete.Values.time = bidsout$Blood$Discrete$Data$Values[,1] +
      0.5*bidsout$Blood$Discrete$Data$Values[,2],
    Blood.Discrete.Values.activity = bidsout$Blood$Discrete$Data$Values[,3],
    Plasma.Values.time = bidsout$Plasma$Data$Values[,1] +
      0.5*bidsout$Plasma$Data$Values[,2],
    Plasma.Values.activity = bidsout$Plasma$Data$Values[,3],
    Metabolite.Values.time = bidsout$Metabolite$Data$Values[,1] +
      0.5*bidsout$Metabolite$Data$Values[,2],
    Metabolite.Values.parentFraction = bidsout$Metabolite$Data$Values[,3],
    Blood.Continuous.Values.time = bidsout$Blood$Continuous$Data$Values[,1],
    Blood.Continuous.Values.activity = bidsout$Blood$Continuous$Data$Values[,2],
    Blood.Continuous.DispersionConstant = Blood_Continuous$DispersionConstant,
    Blood.Continuous.DispersionCorrected = FALSE,
    TimeShift = 0)


  return(blooddata)

}

#' Extract information from Ecat7 files
#'
#' Extracts header, decay, counts and timing information from ecat7 .v files.
#'
#' @param v_filename Filename of the input .v file. Preferably without file
#'   extension. Should only be the filename. Path goes into inpath.
#' @param inpath Path to the input file. Defaults to the working directory.
#' @param outpath Path to which data can be written. Just required for temporary
#'   files. Defaults to the working directory.
#' @param checkLines Should the system commands be checked (and not run)?
#'   Default FALSE.
#'
#' @return If checkLines is TRUE, the commands will be returned.  If checkLines
#'   is FALSE, the relevant data will be returned in a list.
#' @export
#'
#' @examples
#' ecat_info('r_abcd_1', checkLines = F)
#'
ecat_info <- function(v_filename, inpath = getwd(), outpath = getwd(), checkLines = F) {

  # Fix extentions

  v_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(v_filename, "\\.")[[1]])

  if (nrow(v_filename_extensions) > 0) {
    v_extstart <- min(v_filename_extensions$start)
    v_filename <- stringr::str_sub(v_filename, end = v_extstart - 1)
  }

  # Fix Paths

  inpath <- nicebracketer(inpath)
  outpath <- nicebracketer(outpath)

  # Write Commands

  command_hdr <- paste0(
    "lmhdr ",
    '"', inpath, "/", v_filename, ".v", '"'
  )

  command_decay <- paste0(
    "imgdecay ",
    '"', inpath, "/", v_filename, ".v", '"'
  )

  command_sif <- paste0(
    "imgweigh ",
    '"', inpath, "/", v_filename, ".v", '" ',
    '"', outpath, "/", "sifdat.sif", '"'
  )


  if (checkLines) {
    command_hdr <- paste0("echo ", command_hdr)
    command_decay <- paste0("echo ", command_decay)
    command_sif <- paste0("echo ", command_sif)
  }

  # Execute

  outcome_hdr <- system(command_hdr, intern = T)
  outcome_decay <- system(command_decay, intern = T)
  outcome_sif <- system(command_sif, intern = T)

  if (!checkLines) {

    # Munging

    ## Header
    hdr_dat <- terminal_munge(outcome_hdr, matchpattern = "(.*)\\:\\= (.*)")$listdat


    ## Decay Details
    decaydetail_dat <- terminal_munge(outcome_decay[2:4], "(.+): (.+)")$listdat
    decaydetail_dat$Decay_correction <- ifelse(decaydetail_dat$Decay_correction == "corrected",
      1, 0
    )

    ## Decay Factor
    decayfactor_dat <- suppressWarnings(
      terminal_munge(outcome_decay[5:length(outcome_decay)], splitpattern = " +")
    )$tibdat
    decayfactor_dat <- dplyr::select(decayfactor_dat, everything(), Decay.factor = Decay, -factor)

    decayfactor <- decayfactor_dat$Decay.factor


    ## Timing Data
    time_dat <- dplyr::select(decayfactor_dat, Frame, start, length)
    time_dat <- dplyr::mutate(
      time_dat,
      start = start / 60,
      length = length / 60,
      end = start + length,
      midtime = start + length / 2
    )


    ## SIF
    sif_dat <- read.delim(paste0(outpath, "/sifdat.sif"), skip = 1, header = F, sep = " ")
    counts_dat <- dplyr::select(sif_dat, Counts = V3, Randoms = V4)
    file.remove(paste0(outpath, "/sifdat.sif"))


    # Putting together

    out <- list(
      hdr_dat = hdr_dat, decaydetail_dat = decaydetail_dat,
      decayfactor = decayfactor,
      time_dat = time_dat,
      counts_dat = counts_dat
    )
  } else {
    out <- paste0(outcome_hdr, outcome_decay, outcome_sif, sep = "; ")
  }

  return(out)
}


#' Ecat7 to NIfTI Converter
#'
#' Wrapper for the Turku PET Centre ecat2nii function
#'
#' @param v_filename Filename of the input .v file. Preferably without file
#'   extension. Should only be the filename. Path goes into inpath.
#' @param inpath Path to the input file. Defaults to working directory.
#' @param out_filename Filename of the output file. Preferably without file
#'   extension. Defaults to same as the input filename. Should only be the
#'   filename. Path goes into outpath.
#' @param outpath Path where the output file should be placed. Defaults to the
#'   working directory.
#' @param checkLines Should the system commands be checked (and not run)?
#'   Default FALSE.
#' @param compressFile Should the output be compressed as a .nii.gz file?
#'   Defaulse TRUE.
#'
#' @return If checkLines is TRUE, the commands will be returned.  If checkLines
#'   is FALSE, the binary success of each command will be returned.
#' @export
#'
#' @examples
#' ecat2nii('r_abcd_1', out_filename = 'abcd_1', checkLines = F, compressFile = T)
#'
ecat2nii <- function(v_filename, inpath = getwd(), out_filename = NULL,
                     outpath = getwd(), checkLines = F, compressFile = T) {

  # Fix extensions

  v_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(v_filename, "\\.")[[1]])
  out_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(out_filename, "\\.")[[1]])

  if (nrow(v_filename_extensions) > 0) {
    v_extstart <- min(v_filename_extensions$start)
    v_filename <- stringr::str_sub(v_filename, end = v_extstart - 1)
  }

  if (is.null(out_filename)) {
    out_filename <- v_filename
  }

  if (nrow(out_filename_extensions) > 0) {
    out_extstart <- min(out_filename_extensions$start)
    out_filename <- stringr::str_sub(out_filename, end = out_extstart - 1)
  }



  # Fix Paths

  inpath <- nicebracketer(inpath)
  outpath <- nicebracketer(outpath)


  # Write Commands

  command <- paste0(
    "ecat2nii ",
    "-O=", '"', outpath, '"', " ",
    '"', inpath, "/", v_filename, ".v", '"'
  )

  zipcommand <- paste0(
    "gzip ",
    "-f -v ", # Options
    outpath, "/", out_filename, ".nii"
  )

  zipcommand <- ifelse(compressFile,
    yes = zipcommand, no = ""
  )

  if (checkLines) {
    command <- paste0("echo ", command)
    zipcommand <- paste0("echo ", zipcommand)
  }


  # Execute

  outcome <- system(command, intern = T)

  outcome <- paste(outcome, collapse = " ")

  # Returning stuff

  if (checkLines) {

    # Do for checkLines

    print(outcome)

    if (compressFile) {
      zipoutcome <- system(zipcommand, intern = T)
      print(zipoutcome)
      outcome <- paste(outcome, zipoutcome, sep = "; ")
    }

    return(outcome)
  } else {

    # Do if not checkLines
    print(outcome)

    ## Check Success

    strtail <- stringr::str_sub(outcome, start = -10)
    success <- ifelse(strtail == "processed.", 1, 0)

    ## Rename if desired

    if (!is.null(out_filename)) {
      file.rename(
        from = paste0(outpath, "/", v_filename, ".nii"),
        to = paste0(outpath, "/", out_filename, ".nii")
      )
    }


    ## Zip if desired

    if (compressFile) {

      ### Execute
      zipoutcome <- system(zipcommand, intern = T)
      print(zipoutcome)

      ### Check Success
      ziptail <- stringr::str_sub(zipoutcome, start = -7)
      zipsuccess <- ifelse(ziptail == ".nii.gz", 1, 0)
      success <- paste0(success, zipsuccess)
    }


    return(success)
  }
}



#' Get studydb data for a study folder
#'
#' Extract studydb data for a whole folder of subjects
#'
#' @param studyFolder The folder location of the study folder. Default is
#'   current directory.
#' @param savecsv Should a csv file be saved? Default is FALSE. If you wish to
#'   save a csv file, replace this argument with the desired filename.
#'
#' @return A data.frame with the data from the studydb files.
#' @export
#'
#' @examples
#' get_studydb_data_folder(savecsv = 'studydata.csv')
#'
get_studydb_data_folder <- function(studyFolder = getwd(), savecsv = F) {
  dirs <- list.dirs(recursive = F, full.names = F, path = studyFolder)
  dbdat <- lapply(dirs, get_studydb_data, path = studyFolder)
  dbdat <- do.call("rbind", dbdat)

  if (savecsv != F) {
    if (is.character(savecsv)) {
      readr::write_csv(dbdat, path = savecsv)
    } else if (is.logical(savecsv)) {
      readr::write_csv(dbdat, path = "out.csv")
    } else {
      warning("savecsv should either be a logical or a character")
    }
  }

  return(dbdat)
}

#' Get studydb data for a single subject's folder
#'
#' For doing this for a whole study folder, use the *get_studydb_data_folder* command
#'
#' @param subjFolder The name of the subject folder.
#' @param path The file path of the study folder.
#'
#' @return A data.frame with relevant information from the studyDB file.
#' @export
#'
#' @examples
#' abcd_dat <- get_studydb_data('abcd')
#'
get_studydb_data <- function(subjFolder, path = getwd()) {
  path <- nicebracketer(path)
  studydb_file <- paste0(path, "/", subjFolder, "/", "studyDB.mat")

  if (!file.exists(studydb_file)) {
    warning(paste0("No studydb file found for ", subjFolder))
    return(NULL)
  } else {
    dat <- R.matlab::readMat(studydb_file)

    subjName <- as.character(dat$study$SubjectName)
    subjCode <- as.character(dat$study$SubjectCode)

    layoutFile <- as.character(dat$study$layoutFileName)
    variablesFile <- as.character(dat$study$variablesFileName)

    petfolder <- nicebracketer(dat$study$raw.pet.ecat7.dir)
    mrfolder <- nicebracketer(dat$study$raw.mr.dicom.dir)


    variables <- dat$study$variables[[1]][[1]][, , 1]

    petvariables <- unlist(variables$PET[,,1])

    petfiles <- tibble::tibble(
      modality = "pet",
      descrip = names(petvariables),
      filenames = paste0(petfolder, "/", unlist(petvariables))
    )

    petfiles$PETNo <- stringr::str_extract(petfiles$descrip, "[1-9]$")

    mrfiles <- tibble::tibble(
      modality = "mr",
      descrip = "dicomfolder",
      filenames = paste0(mrfolder, "/", unlist(variables$MR))
    )

    outdat <- dplyr::bind_rows(petfiles, mrfiles)

    outdat$sdir <- tools::file_path_as_absolute(path)
    outdat$subjFolder <- subjFolder
    outdat$subjName <- subjName
    outdat$subjCode <- subjCode
    outdat$layoutFile <- layoutFile
    outdat$variablesFile <- variablesFile

    outdat <- dplyr::select(
      outdat, subjName, subjCode,
      modality, descrip, PETNo,
      sdir, subjFolder, filenames,
      layoutFile, variablesFile
    )

    return(outdat)
  }
}

#' Nice Bracketer
#'
#' Because Windows brackets look ugly for folder locations
#'
#' @param filelocation A path name
#'
#' @return The same path name, but with forward slashes
#' @export
#'
#' @examples
#' nicebracketer('C:\\Users\\User\\Documents\\SpecialFile.docx')
#' nicebracketer('C:\\Users\\User\\Documents')
#'
#'
nicebracketer <- function(filelocation) {
  filelocation <- as.character(filelocation)
  out <- gsub(pattern = "\\", "/", filelocation, fixed = T)
  return(out)
}


#' Munge terminal output
#'
#' Split and produce data from terminal output
#'
#' @param terminal_output Raw terminal output. Make sure that it consists of one
#'   data rectangle.
#' @param matchpattern Regex pattern for dividing the data set. Uses
#'   stringr::str_match. Specify either matchpattern or splitpattern.
#' @param splitpattern Regex pattern for splitting the data set. Uses
#'   stringr::str_split. Specify either splitpattern or matchpattern.
#' @param colnames What should the column names of the tibble? If the terminal
#'   output is a list of attributes and their values, then it will be handled
#'   automatically.
#' @param has_header Does the table have a header inside it. Default is TRUE.
#'
#' @return A list including a tibble of the output, and, if the output consists
#'   of attributes and values, then it will also include a list.
#' @export
#'
#' @examples
#' hdr_dat <- terminal_munge(outcome_hdr, matchpattern='(.*)\\:\\= (.*)')
#'
terminal_munge <- function(terminal_output, matchpattern=NULL, splitpattern=NULL, colnames = NULL, has_header=T) {
  if (is.null(matchpattern) & is.null(splitpattern)) stop("Specify either matchpattern or splitpattern.")
  if (!is.null(matchpattern) & !is.null(splitpattern)) stop("Specify either matchpattern or splitpattern.")

  terminal_output <- stringr::str_trim(terminal_output)

  if (!is.null(matchpattern)) {
    to_unstringed <- stringr::str_match(terminal_output, pattern = matchpattern)
    to_dat <- tibble::as_tibble(to_unstringed)[, -1]
  }

  if (!is.null(splitpattern)) {
    to_unstringed <- stringr::str_split(terminal_output, pattern = splitpattern)
    to_unstringed <- do.call(rbind, to_unstringed)
    to_dat <- tibble::as_tibble(to_unstringed)

    if (!is.null(colnames)) {
      colnames(to_dat) <- colnames
    }

    if (has_header & is.null(colnames)) {
      colnames(to_dat) <- to_dat[1, ]
      to_dat <- to_dat[-1, ]
    }
  }

  to_dat <- dplyr::mutate_all(to_dat, stringr::str_trim)
  to_dat <- dplyr::mutate_all(to_dat, type.convert)
  to_dat <- dplyr::mutate_if(to_dat, is.factor, as.character)

  out <- list(tibdat = to_dat)

  if (ncol(to_dat) == 2) {
    colnames(to_dat) <- c("attr", "value")
    to_dat$attr <- gsub(to_dat$attr, pattern = " ", replacement = "_")
    to_listdat <- split(to_dat$value, f = to_dat$attr)

    to_listdat <- lapply(to_listdat, type.convert)
    to_listdat <- lapply(to_listdat, function(x) ifelse(is.factor(x), as.character(x), x))

    out$listdat <- to_listdat
  }

  return(out)
}

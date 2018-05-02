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

  inpath <- normalizePath(inpath, winslash = "/")
  outpath <- normalizePath(outpath, winslash = "/")

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
    decayfactor_dat <- dplyr::select(decayfactor_dat, -factor)
    decayfactor_dat <- dplyr::rename(decayfactor_dat, Decay.factor = Decay)

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
    out <- paste(outcome_hdr, outcome_decay, outcome_sif, sep = "; ")
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
#'   Default TRUE.
#' @param bids_sidecar Should a BIDS sidecar file also be produced? Default T.
#'
#' @return If checkLines is TRUE, the commands will be returned.  If checkLines
#'   is FALSE, the binary success of each command will be returned.
#' @export
#'
#' @examples
#' ecat2nii('r_abcd_1', out_filename = 'abcd_1', checkLines = F, compressFile = T)
#'
ecat2nii <- function(v_filename, inpath = getwd(), out_filename = NULL,
                     outpath = getwd(), checkLines = F, compressFile = T,
                     bids_sidecar = T) {

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

  inpath <- normalizePath(inpath, winslash = "/")
  outpath <- normalizePath(outpath, winslash = "/")


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

    if (bids_sidecar) {
      ecat_info2bids_json(
        v_filename, out_filename, inpath,
        outpath
      )
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
#' @param pet_orig_filenames Extract original PET filenames. Note that this takes a while
#'  as it requires opening the ecat7 fiels.
#'
#' @return A data.frame with the data from the studydb files.
#' @export
#'
#' @examples
#' get_studydb_data_folder(savecsv = 'studydata.csv')
#'
get_studydb_data_folder <- function(studyFolder = getwd(), savecsv = F, pet_orig_filenames = T) {
  dirs <- list.dirs(recursive = F, full.names = F, path = studyFolder)
  dbdat <- lapply(
    dirs, get_studydb_data, path = studyFolder,
    pet_orig_filenames = pet_orig_filenames
  )
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
#' @param pet_orig_filenames Extract original PET filenames. Note that this takes a while
#'  as it requires opening the ecat7 fiels.
#'
#' @return A data.frame with relevant information from the studyDB file.
#' @export
#'
#' @examples
#' abcd_dat <- get_studydb_data('abcd')
#'
get_studydb_data <- function(subjFolder, path = getwd(), pet_orig_filenames = T) {
  path <- normalizePath(path, winslash = "/")
  studydb_file <- paste0(path, "/", subjFolder, "/", "studyDB.mat")

  if (!file.exists(studydb_file)) {
    warning(paste0("No studydb file found for ", subjFolder, "\n"))
    return(NULL)
  } else {
    dat <- R.matlab::readMat(studydb_file)

    subjName <- as.character(dat$study$SubjectName)
    subjCode <- as.character(dat$study$SubjectCode)

    layoutFile <- as.character(dat$study$layoutFileName)
    variablesFile <- as.character(dat$study$variablesFileName)

    petfolder <- gsub("\\", "/", dat$study$raw.pet.ecat7.dir, fixed = T)
    mrfolder <- gsub("\\", "/", dat$study$raw.mr.dicom.dir, fixed = T)


    variables <- dat$study$variables[[1]][[1]][, , 1]


    # PET Data

    petvariables <- unlist(variables$PET[, , 1])

    petfiles <- tibble::tibble(
      modality = "pet",
      descrip = names(petvariables),
      subjRelativePath = as.character(petfolder),
      filenames = unlist(petvariables)
    )

    petfiles$PETNo <- stringr::str_extract(petfiles$descrip, "[1-9]$")

    ## Get original filenames

    if (pet_orig_filenames) {
      ecatinfo <- tidyr::nest(petfiles, -descrip)
      ecatinfo <- dplyr::mutate(ecatinfo, ecatdata = purrr::map(ecatinfo$data, ~ecat_info(
        v_filename = .x$filenames,
        inpath = paste(path, subjFolder, .x$subjRelativePath, sep = "/")
      )))
      orig_filenames <- purrr::map_chr(ecatinfo$ecatdata, c("hdr_dat", "original_file_name"))
      orig_filenames <- stringr::str_match(orig_filenames, pattern = "([\\w]*)[\\.[\\w]]*$")[, 2]

      petfiles$orig_filenames <- orig_filenames
    } else {
      petfiles$orig_filenames <- NA
    }





    # MR Data

    mrfiles <- tibble::tibble(
      modality = "mr",
      descrip = "dicomfolder",
      subjRelativePath = as.character(mrfolder),
      filenames = unlist(variables$MR)
    )



    # Preparing for output

    outdat <- dplyr::bind_rows(petfiles, mrfiles)

    outdat$studyFolder <- tools::file_path_as_absolute(path)
    outdat$subjFolder <- subjFolder
    outdat$subjName <- subjName
    outdat$subjCode <- subjCode
    outdat$layoutFile <- layoutFile
    outdat$variablesFile <- variablesFile

    outdat <- dplyr::select(
      outdat, subjName, subjCode,
      modality, descrip, PETNo,
      studyFolder, subjFolder,
      subjRelativePath,
      filenames, orig_filenames,
      layoutFile, variablesFile
    )

    return(outdat)
  }
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


#' Get Attributes of DICOM files
#'
#' This function allows users to select which DICOM folder to convert for
#' whichever purpose.  Heuristic strategies can be used to select from this list
#'
#' @param inpath The path where the DICOM files are located.  A file search will
#'   take place in all nested directories. This allows for convenience in
#'   directory specification, but be careful about having lots of files in
#'   nested places within the specified folder.
#'
#' @return A tibble of the details and attributes of the found dcm files.
#' @export
#'
#' @examples
#' dicom_attributes()
dicom_attributes <- function(inpath = getwd()) {
  inpath <- normalizePath(inpath, winslash = "/")

  dcms <- list.files(inpath, pattern = "*.dcm", recursive = T, full.names = F)
  dcm_filedetails <- stringr::str_split(dcms, "/", simplify = T)


  # This is an ugly fix - something more elegant should be here
  if (ncol(dcm_filedetails) > 3) {
    extraPath <- apply(
      dcm_filedetails[, 1:(ncol(dcm_filedetails) - 2)],
      1, paste, collapse = "/"
    )
    extraPath <- paste0("/", extraPath)
  }

  if (ncol(dcm_filedetails) > 2) {
    extraPath <- paste0("/", dcm_filedetails[, (ncol(dcm_filedetails) - 2)])
  }

  if (ncol(dcm_filedetails) <= 2) {
    extraPath <- NULL
  }

  dcm_files <- tibble::tibble(
    dcm_foldername = dcm_filedetails[, (ncol(dcm_filedetails) - 1)],
    dcm_filename = dcm_filedetails[, ncol(dcm_filedetails)],
    mainpath = paste0(inpath, extraPath),
    folderpath = paste0(inpath, extraPath, "/", dcm_foldername)
  )

  first_dcms <- which(!duplicated(dcm_files$dcm_foldername, fromLast = F))
  dcm_folders <- dcm_files[first_dcms, ]

  dcm_details <- lapply(dcm_folders$folderpath, divest::scanDicom)
  dcm_details <- dplyr::bind_rows(dcm_details)
  dcm_details <- dplyr::rename(dcm_details, folderpath = rootPath)

  dcm_details <- dplyr::full_join(dcm_details, dcm_folders)

  return(dcm_details)
}


#' DICOM 2 NII Converter with BIDS Sidecars
#'
#' Function to convert DCM to nii, including the BIDS sidecar.  This is a
#' wrapper for dcm2niix by Chris Rorden.
#'
#' @param dcm_folder The folder of the DICOM file which should be converted.
#'   This can either be the whole path, or it can be the specific foldername
#'   (with the path in inpath).
#' @param inpath Optional: this is the path of the folder which contains the
#'   folder with the DICOM files.  If the full path is specified as dcm_folder,
#'   then this argument should be left.
#' @param out_filename Filename of the output file. Preferably without file
#'   extension. Defaults to same as the input filename. Should only be the
#'   filename. Path goes into outpath. Can also be specified in terms of the
#'   dcm2niix guidelines.
#' @param outpath Path where the output file should be placed. Defaults to the
#'   working directory.
#' @param checkLines hould the system commands be checked (and not run)? Default
#'   FALSE.
#' @param compressFile Should the output be compressed as a .nii.gz file?
#'   Default TRUE.
#' @param bids_sidecar Should the BIDS sidecar json file be produced. Options
#'   are 'y', 'n' or 'o' (only).
#' @param anon Should the BIDS sidecar be anonymised? Options are 'y' or 'n'
#'
#' @return Creates the nii and json files as desired and returns the success
#'   message.
#' @export
#'
#' @references https://github.com/rordenlab/dcm2niix
#'
#' @examples
#' dicom2nii(dcm_folder, checkLines = T)
dicom2nii <- function(dcm_folder, inpath = NULL, out_filename = "%f_%p_%t_%s",
                      outpath = getwd(), checkLines = F, compressFile = "y",
                      bids_sidecar="y", anon="y") {

  # Fix extensions

  out_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(out_filename, "\\.")[[1]])

  if (nrow(out_filename_extensions) > 0) {
    out_extstart <- min(out_filename_extensions$start)
    out_filename <- stringr::str_sub(out_filename, end = out_extstart - 1)
  }



  # Fix Paths

  if (!is.null(inpath)) {
    inpath <- normalizePath(inpath, winslash = "/")
    dcm_folder <- paste0(inpath, "/", dcm_folder)
  } else {
    dcm_folder <- normalizePath(dcm_folder, winslash = "/")
  }

  outpath <- normalizePath(outpath, winslash = "/")
  dir.create(outpath)


  # Write Commands

  command <- paste0(
    "dcm2niix ",
    "-b ", bids_sidecar, " ",
    "-ba ", anon, " ",
    "-z ", compressFile, " ",
    "-o ", '"', outpath, '"', " ",
    "-f ", out_filename, " ",
    '"', dcm_folder, '"'
  )

  if (checkLines) {
    command <- paste0("echo ", command)
  }


  # Execute

  outcome <- system(command, intern = T)

  outcome <- paste(outcome, collapse = " ")

  print(outcome)

  return(outcome)
}


# gradnonlincorr_check <- function(img_filename, inpath = getwd(), img_format_override = NULL) {
#
#   # Fix extentions
#
#   img_format <- NULL
#
#   img_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(img_filename, "\\.")[[1]])
#
#   if (nrow(img_filename_extensions) > 0) {
#     img_extstart <- min(img_filename_extensions$start)
#     img_format <- stringr::str_sub(img_filename, start = img_extstart + 1)
#     img_filename <- stringr::str_sub(img_filename, end = img_extstart - 1)
#   }
#
#
#   # If still NULL (i.e. not in filename)
#   if (is.null(img_format)) {
#
#     if( file_test("-d", img_filename) ) {
#       img_format <- 'dcm'
#     }
#
#     if( file_test("-f", paste0(img_filename, '.json')) ) {
#       img_format='nii'
#     }
#
#     # If it's still null (i.e. no DICOM or BIDS sidecar)
#
#     if(is.null(img_format) && is.null(img_format_override)) {
#       stop("Cannot figure out the format of your file. It should be nifti or dicom. If
#            its nifti, then there should be a bids sidecar.")
#     }
#
#   }
#
#   # Fix Paths
#
#   inpath <- normalizePath(inpath, winslash = "/")
#
#   # DCM file
#   if( img_format <- 'dcm' ) {
#     attributes <- dicom_attributes(paste0(inpath, '/', img_filename))
#   }
#
# }


#' Perform Gradient Nonlinearity Correction
#'
#' This function wraps around FreeSurfer, with MATLAB installed, and the dev
#' MATLAB files in the correct folders. I've written a guide to installing this
#' which I'll hopefully put online soon, but contact me if you need this function
#' and don't have it.  This function modifies the file in place, and saves everything else either in the same folder, or elsewhere.
#'
#' @param nii_filename Filename of the input nii file. With or without file
#' extension
#' @param inpath Path to the input file. Defaults to working directory.
#' @param coeff_file Filename of the coefficient file. It should be placed in a special folder and named a particular way, but this information is or will be in the setup guide.
#' @param outpath_checks Path for the extra images for checking to be placed into. If not specified, they will be kept in the same folder.
#' @param checkLines Should the system commands be checked (and not run)?
#'   Default FALSE.
#' @param bids_sidecar_heurcheck If it is not explicitly stated in the sidecar whether correction has been applied, this checks the sidecar for heuristic signs. This is, however, not guaranteed to come to the correct answer, depending on the MR system. Default is TRUE.
#' @param bids_sidecar_exists Defaults to TRUE. Set this to FALSE in case of there not being a BIDS sidecar.
#'
#' @return This function modifies the file in place, saves all the other files to check, and returns the outcome from the terminal.
#' @export
#'
#' @examples
#' gradnonlincorr("sub-01_T1w", coeff_file='coeff_avanto.grad')
#'
gradnonlincorr <- function(nii_filename, inpath = getwd(), coeff_file,
                           outpath_checks = NULL, checkLines = F,
                           bids_sidecar_heurcheck = T, bids_sidecar_exists = T) {

  # Fix extentions

  nii_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(nii_filename, "\\.")[[1]])

  nii_extension <- NULL

  if (nrow(nii_filename_extensions) > 0) {
    nii_extstart <- min(nii_filename_extensions$start)
    nii_extension <- stringr::str_sub(nii_filename, start = nii_extstart + 1)
    nii_filename <- stringr::str_sub(nii_filename, end = nii_extstart - 1)
  }

  # Fix Paths

  inpath <- normalizePath(inpath, winslash = "/")

  # BIDS sidecar

  if (!bids_sidecar_exists) {
    bids_sidecar_heurcheck <- F
  }

  if (bids_sidecar_exists) {
    bids_sc <- jsonlite::fromJSON(paste0(inpath, "/", nii_filename, ".json"))

    if ("NonlinearGradientCorrection" %in% names(bids_sc)) {
      if (bids_sc$NonlinearGradientCorrection == 1) {
        system(stringr::str_glue("echo Gradient Correction already applied for {nii_filename}"))
        return()
      }
    }
  }

  # Check BIDS sidecar using heuristic checks

  if (bids_sidecar_heurcheck) {

    # Checking Image Type field
    imagetype_uncorr_tags <- c("ND") # Tags suggesting uncorrected in ImageType

    imagetype_uncorr_tag_outcomes <- purrr::map(
      imagetype_uncorr_tags,
      function(x) stringr::str_match(bids_sc$ImageType, x)
    )
    imagetype_uncorr_tag_outcomes <- purrr::map_dbl(
      imagetype_uncorr_tag_outcomes,
      function(x) sum(!is.na(x))
    )

    # Can add other fields to check for other non-Siemens systems here

    # Combine them all here
    outcomes <- sum(c(imagetype_uncorr_tag_outcomes))

    if (outcomes == 0) {
      system(stringr::str_glue(
        "echo Gradient Correction does not appear necessary for {nii_filename}. ",
        "This check was heuristic though, and could be wrong. Use ",
        "bids_sidecar_heurcheck = F to override this test"
      ))
      return()
    }
  }

  # Now we perform correction if we reach this point

  ## Figuring out extensions

  if (is.null(nii_extension)) {
    if (file_test("-f", paste0(nii_filename, ".nii"))) {
      nii_extension <- "nii"
    }

    if (file_test("-f", paste0(nii_filename, ".nii.gz"))) {
      nii_extension <- "nii.gz"
    }

    # If it's still null (i.e. no DICOM or BIDS sidecar)

    if (is.null(nii_extension)) {
      stop("Cannot figure out the file format of the nii file. Please include the
           extension in the filename")
    }
  }

  # Writing the command

  command <- stringr::str_glue(
    "gradient_nonlin_unwarp.sh ",
    "{inpath}/{nii_filename}.{nii_extension} ",
    "{inpath}/{nii_filename}.{nii_extension} ",
    "{coeff_file} --nobiascor --shiftmap"
  )

  if (checkLines) {
    command <- paste0("echo ", command)
  } else {

    # First, save the original
    file.copy(
      from = stringr::str_glue("{inpath}/{nii_filename}.{nii_extension}"),
      to = stringr::str_glue("{inpath}/{nii_filename}__original.{nii_extension}")
    )
  }



  # Execute
  outcome <- system(command, intern = T)


  # Extra things if command was executed
  if (!checkLines) {

    # Add field to sidecar
    if (bids_sidecar_exists) {
      add2json(
        list(NonlinearGradientCorrection = 1),
        json_filename = stringr::str_glue("{inpath}/{nii_filename}.json")
      )
    }

    # Other files
    if (!is.null(outpath_checks)) {
      if (!dir.exists(outpath_checks)) {
        dir.create(outpath_checks)
      }

      file.rename(
        from = stringr::str_glue("{inpath}/{nii_filename}__deform_grad_abs.nii.gz"),
        to = stringr::str_glue("{outpath_checks}/{nii_filename}__deform_grad_abs.nii.gz")
      )

      file.rename(
        from = stringr::str_glue("{inpath}/{nii_filename}__deform_grad_rel.nii.gz"),
        to = stringr::str_glue("{outpath_checks}/{nii_filename}__deform_grad_rel.nii.gz")
      )

      file.rename(
        from = stringr::str_glue("{inpath}/{nii_filename}__original.{nii_extension}"),
        to = stringr::str_glue("{outpath_checks}/{nii_filename}__original.{nii_extension}")
      )

      file.rename(
        from = stringr::str_glue("{inpath}/{nii_filename}__gradient_nonlin_unwarp.log"),
        to = stringr::str_glue("{outpath_checks}/{nii_filename}__gradient_nonlin_unwarp.log")
      )
    }
  }

  return(outcome)
}

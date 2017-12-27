#' Extract Information from ecat7 Header into BIDS Sidecar
#'
#' @param v_filename Filename of the input .v file. Preferably without file
#'   extension. Should only be the filename. Path goes into inpath.
#' @param out_filename Filename of the output file. Preferably without file
#'   extension. Should only be the filename. Path goes into outpath.
#' @param inpath Path to the input file. Defaults to the working directory.
#' @param outpath Path to the output file. Defaults to the working directory.
#'
#' @return Produces the PET BIDS sidecar with all relevant information from the
#'   .v file
#' @export
#'
#' @examples
#' ecat_info2bids_json('abcd_1', 'sub-01_ses-2015_task-rest_acq-T807AV1451_pet')
#'
ecat_info2bids_json <- function(v_filename, out_filename, inpath = getwd(),
                                outpath = getwd()) {


  # Fix extensions

  v_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(v_filename, "\\.")[[1]])
  out_filename_extensions <- tibble::as_tibble(stringr::str_locate_all(out_filename, "\\.")[[1]])

  if (nrow(v_filename_extensions) > 0) {
    v_extstart <- min(v_filename_extensions$start)
    v_filename <- stringr::str_sub(v_filename, end = v_extstart - 1)
  }

  if (nrow(out_filename_extensions) > 0) {
    out_extstart <- min(out_filename_extensions$start)
    out_filename <- stringr::str_sub(out_filename, end = out_extstart - 1)
  }

  # Fix Paths

  inpath <- normalizePath(inpath, winslash = '/')
  outpath <- normalizePath(outpath, winslash = '/')


  # Get Info

  ecatinfo <- ecat_info(paste0(v_filename, ".v"), inpath, outpath)

  #############################################################################

  # Info

  ## Tracer Info
  Info_Tracer <- list(
    Isotope = stringr::str_replace(ecatinfo$hdr_dat$isotope_name, "[^A-Za-z0-9]", ""),
    Name = stringr::str_replace(ecatinfo$hdr_dat$radiopharmaceutical, "^.+ ", "")
  )

  Info <- list(
    Tracer = Info_Tracer
  )

  #############################################################################

  # Time

  ## FrameTimes

  Value_matrix <- dplyr::select(ecatinfo$time_dat, start, end)
  Value_matrix <- data.matrix(Value_matrix) * 60
  colnames(Value_matrix) <- NULL

  Time_FrameTimes <- list(
    Labels = c("frameStart", "frameEnd"),
    Units = c("s", "s"),
    Values = Value_matrix
  )

  ScanStart <- lubridate::as_datetime(ecatinfo$hdr_dat$scan_start_time)
  ScanStart_time <- strftime(ScanStart, format = "%H:%M:%S", tz = "UTC")



  Time <- list(
    ScanStart = ScanStart_time,
    ScanStartUnits = "hh:mm:ss",
    FrameTimes = Time_FrameTimes
  )

  #############################################################################

  # Write Output

  out <- list(Info = Info, Time = Time)

  # json_out <- jsonlite::toJSON(out)
  # json_out <- jsonlite::prettify(json_out)

  jsonlite::write_json(
    out,
    path = paste0(outpath, "/", out_filename, ".json"),
    pretty=T
  )
}

#' Combine JSON files
#'
#' @param json_filenames Vector of the json files to be combined. Preferably
#'   without file extensions. Preferably containing the full path.
#' @param out_filename Filename of the output json file.  Preferably without
#'   file extensions.
#' @param inpath Path to the input files. Defaults to the working directory.
#' @param outpath Path to the output files. Defaults to the working directory.
#'
#' @return Creates a combined .json file.
#' @export
#'
#' @examples
#' k <- list(abc = 1, def = 2)
#' j <- list(abcd = 3, defg = 4)
#' jsonlite::write_json(k, 'json1.json')
#' jsonlite::write_json(j, 'json2.json')
#'
#' combine_jsons(c('json1', 'json2'), 'combined')
combine_jsons <- function(json_filenames, out_filename, outpath = getwd()) {

  # Fix extensions

  json_filenames <- purrr::map(json_filenames, stringr::str_split_fixed, pattern='\\.', n=2)
  json_filenames <- do.call(rbind, json_filenames)[,1]

  out_filename <- stringr::str_split_fixed(out_filename, '\\.', 2)[[1]]

  json_filenames <- paste0(json_filenames, '.json')
  out_filename <- paste0(out_filename, '.json')

  # Read and combine

  json_data <- lapply(json_filenames, jsonlite::fromJSON )
  combined_json <- do.call(c, json_data)

  # Write

  jsonlite::write_json(
    combined_json,
    path = paste0(outpath, "/", out_filename),
    pretty=T
  )

}

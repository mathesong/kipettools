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

  inpath <- nicebracketer(inpath)
  outpath <- nicebracketer(outpath)


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

  json_out <- jsonlite::toJSON(out)
  json_out <- jsonlite::prettify(json_out)

  jsonlite::write_json(
    json_out,
    path = paste0(outpath, "/", out_filename, ".json")
  )
}

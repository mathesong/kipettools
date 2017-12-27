#' Get the OS
#'
#' Note: This function is copied directly from https://github.com/r-lib/rappdirs/blob/master/R/utils.r#L1
#'
#' @return The OS: win/mac/unix
#' @export
#'
#' @references https://github.com/r-lib/rappdirs/blob/master/R/utils.r#L1
#'
#' @examples
#' get_os()
#'
get_os <- function() {

  # This function is copied directly from
  # https://github.com/r-lib/rappdirs/blob/master/R/utils.r#L1

  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

#' Check if the Windows Ubuntu extension is installed and available
#'
#' @return Logical of whether Ubuntu can be called from Windows
#' @export
#'
#' @examples
#' check_winlinux()
#'
check_winlinux <- function() {
  success <- tryCatch(expr = { system('bash -c uname -a', intern=T)=="Linux" },
                      error = function(e) { return(FALSE) })
  return(success)
}


#' Check if tpcclib is installed and in the path
#'
#' @return Logical of whether tpcclib was successfully called from the path.
#' @export
#'
#' @examples
#' check_tpcclib()
#'
check_tpcclib <- function() {
  suppressWarnings(
    out <- tryCatch(expr = { paste (system('imghead', intern=T), collapse = ' ') },
                      error = function(e) { return(FALSE) }) )

  success <- ifelse( stringr::str_detect(out, 'tpcclib'), TRUE, FALSE )
  return(success)
}


#' Fix a command such that it can run on Windows Ubuntu
#'
#' @param command_text One or more commands as characters which will be appropriately prefixed.
#'
#' @return The texts with the correct prefixes.
#' @export
#'
#' @examples
#' fix_command_linux('uname -a')
fix_command_linux <- function(command_text) {

  os <- get_os()

  if(os=='win') {
    winlin <- check_winlinux()

    if(!winlin) {
      stop(paste0('Your OS appears to be Windows, but you require Linux for this command.\n',
                  'You can use the Windows 10 Ubuntu extension, however it does not\n',
                  'appear to be available. Please verify this it is installed and\n',
                  'working correctly'))
    }
    command_text <- paste0('bash -c ', command_text)
  }

  return(command_text)

}

#' Check if dcm2niix is installed and in the path
#'
#' @return Logical of whether dcm2niix was successfully called from the path.
#' @export
#'
#' @examples
#' check_dcm2niix()
#'
check_dcm2niix <- function() {

  call_text <- fix_command_linux('dcm2niix')

  suppressWarnings(
    out <- tryCatch(expr = { paste (system(call_text, intern=T), collapse = ' ') },
                    error = function(e) { return(FALSE) }) )

  success <- ifelse( stringr::str_detect(out, 'dcm2niiX'), TRUE, FALSE )

  return(success)
}



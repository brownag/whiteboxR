.onLoad <- function(libname, pkgname) {
  # check_whitebox_binary() is called "loudly" only on package load either:
  #   1. interactively, or 
  #   2. environment var R_WHITEBOX_VERBOSE=TRUE or package option whitebox.verbose=TRUE
  check_whitebox_binary(silent = !wbt_verbose())
}

#' Check for WhiteboxTools executable path
#'
#' @param silent logical. Print help on installation/setting path. Default `TRUE`.
#' @seealso [wbt_exe_path()]
#' @return logical if WhiteboxTools executable file exists.
#' @export
check_whitebox_binary <- function(silent = TRUE) {

  # look in standard locations
  exe_path <- wbt_exe_path(shell_quote = FALSE)
  res <- file.exists(exe_path)

  if (!res && !silent) {
    msg <- paste0(
      "\n",
      "------------------------------------------------------------------------\n",
      "Could not find WhiteboxTools!\n",
      "------------------------------------------------------------------------\n",
      "\n",
      "Your next step is to download and install the WhiteboxTools binary:\n",
      "    > whitebox::install_whitebox()\n",
      "\n",
      "If you have WhiteboxTools installed already run `wbt_init(exe_path=...)`': \n",
      "    > wbt_init(exe_path='/home/user/path/to/whitebox_tools')\n",
      "\n",
      "For whitebox package documentation, ask for help:\n",
      "    > ??whitebox\n",
      "\n",
      "For more information visit https://giswqs.github.io/whiteboxR/\n",
      "\n",
      "------------------------------------------------------------------------\n")
    message(msg)
  }
  res
}

#' @importFrom jsonlite read_json
# returns TRUE if settings.json is present in R working directory, FALSE otherwise
check_settings_json <- function(wd = NULL) {
  if (!requireNamespace('jsonlite'))
    stop('package `jsonlite` is required', call. = FALSE)
  
  # check R working directory for settings.json
  if (file.exists("settings.json")) {
    # read it
    x <- jsonlite::read_json("settings.json")
    
    # get current package wd option
    wbtwd <- wbt_wd()
    
    if (wbtwd != "" & length(wd) > 0) {
      
      # append "/"
      if (!endsWith(wd, "/"))
        wd <- paste0(wd, "/")
      if (!endsWith(wbtwd, "/"))
        wbtwd <- paste0(wbtwd, "/")
      
      # if the package option doesn't match the settings.json working_directory
      #    or the value of `wd` argument (i.e. if wd is changing) then issue a warning
      #    shouldn't happen, but changing package options/settings.json manually could cause it
      if (wbtwd != x$working_directory & getwd() != x$working_directory & wd != wbtwd)
        warning("settings.json in ", getwd(), " has different working_directory (",
                x$working_directory, ") than wbt_wd() (", wbtwd, ")!", call. = FALSE)
      return(TRUE)
    }
  }
  FALSE
}

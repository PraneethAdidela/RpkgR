#' Scan Package Repository
#'
#' Scan a folder for package tar.gz files.
#'
#' @param repo_path Path containing package files.
#'
#' @return Data frame of package files found.
#' @export

pkgr_scan_repo <- function(repo_path) {

  files <- list.files(
    path = repo_path,
    pattern = "\\.(tar\\.gz|zip)$",
    full.names = FALSE
  )

  if(length(files) == 0) {

    return(
      data.frame(
        File = character(),
        stringsAsFactors = FALSE
      )
    )

  }

  data.frame(
    File = files,
    stringsAsFactors = FALSE
  )

}

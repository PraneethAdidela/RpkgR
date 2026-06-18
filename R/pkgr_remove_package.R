#' Remove Package from Repository
#'
#' Removes package files from a repository.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return Removed files.
#' @export

pkgr_remove_package <- function(
    package,
    repo_path
) {

  if(
    missing(package)
  ) {

    stop(
      "package is required."
    )

  }

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  files <- list.files(
    repo_path,
    pattern = paste0(
      "^",
      package,
      "_.*\\.tar\\.gz$"
    ),
    full.names = TRUE
  )

  if(
    length(files) == 0
  ) {

    stop(
      paste(
        "Package not found:",
        package
      )
    )

  }

  removed <- basename(files)

  file.remove(files)

  message(
    length(removed),
    " file(s) removed."
  )

  invisible(
    removed
  )

}

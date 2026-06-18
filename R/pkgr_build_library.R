#' Build Library
#'
#' Creates, installs and activates
#' a local package library.
#'
#' @param package_file Package file.
#' @param library_path Library path.
#'
#' @return Library path.
#' @export

pkgr_build_library <- function(
    package_file,
    library_path
) {

  if(
    missing(package_file)
  ) {

    stop(
      "package_file is required."
    )

  }

  if(
    missing(library_path)
  ) {

    stop(
      "library_path is required."
    )

  }

  pkgr_create_library(
    library_path = library_path
  )

  pkgr_install_library(
    package_file = package_file,
    library_path = library_path
  )

  pkgr_use_library(
    library_path = library_path
  )

  message(
    "Library build completed."
  )

  invisible(
    normalizePath(
      library_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

}

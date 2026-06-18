#' Install Package to Local Library
#'
#' Installs a package into a local library.
#'
#' @param package_file Package file path.
#' @param library_path Library path.
#'
#' @return Invisible NULL.
#' @export

pkgr_install_library <- function(
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

  if(
    !file.exists(package_file)
  ) {

    stop(
      paste(
        "Package file not found:",
        package_file
      )
    )

  }

  if(
    !dir.exists(library_path)
  ) {

    dir.create(
      library_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  utils::install.packages(
    pkgs = package_file,
    repos = NULL,
    type = "source",
    lib = library_path
  )

  invisible(
    NULL
  )

}

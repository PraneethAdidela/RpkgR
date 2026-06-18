#' Install Package from Repository
#'
#' Installs a package and its dependencies
#' from a RpkgR repository into a library.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#' @param library_path Library path.
#'
#' @return Installed packages.
#' @export

pkgr_install_package <- function(
    package,
    repo_path,
    library_path
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

  if(
    missing(library_path)
  ) {

    stop(
      "library_path is required."
    )

  }

  status <- pkgr_repo_status(
    package = package,
    repo_path = repo_path
  )

  if(
    status$Status != "COMPLETE"
  ) {

    stop(
      paste(
        "Repository is incomplete.\n\nMissing files:\n",
        paste(
          status$Missing,
          collapse = "\n"
        )
      )
    )

  }

  bundle <- pkgr_bundle(
    package = package,
    repo_path = repo_path
  )

  repo_files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  installed <- character()

  for(pkg in bundle$Installation_Order) {

    pkg_file <- repo_files[
      grepl(
        paste0("^", pkg, "_"),
        basename(repo_files)
      )
    ]

    if(
      length(pkg_file) == 0
    ) {

      stop(
        paste(
          "Package file not found:",
          pkg
        )
      )

    }

    utils::install.packages(
      pkg_file[1],
      repos = NULL,
      type = "source",
      lib = library_path
    )

    installed <- c(
      installed,
      pkg
    )

  }

  message(
    length(installed),
    " package(s) installed."
  )

  invisible(
    installed
  )

}

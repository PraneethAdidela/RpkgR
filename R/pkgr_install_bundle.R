#' Install Deployment Bundle
#'
#' Installs all package files from a bundle
#' into a target library.
#'
#' @param bundle_path Bundle path.
#' @param library_path Library path.
#' @param validate Validate bundle before installation.
#'
#' @return Installed packages.
#' @export

pkgr_install_bundle <- function(
    bundle_path,
    library_path,
    validate = TRUE
) {

  if(
    missing(bundle_path)
  ) {

    stop(
      "bundle_path is required."
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
    !dir.exists(bundle_path)
  ) {

    stop(
      "Bundle path does not exist."
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

  manifest_file <- list.files(
    bundle_path,
    pattern = "_manifest\\.csv$",
    full.names = TRUE
  )

  if(
    length(manifest_file) == 0
  ) {

    stop(
      "Manifest file not found."
    )

  }

  manifest <- utils::read.csv(
    manifest_file[1],
    stringsAsFactors = FALSE
  )

  missing_files <- character()

  for(
    i in seq_len(
      nrow(manifest)
    )
  ) {

    required_file <- manifest$Required_File[i]

    pattern <- paste0(
      "^",
      gsub(
        "\\*",
        ".*",
        required_file
      ),
      "$"
    )

    package_file <- list.files(
      bundle_path,
      pattern = pattern,
      full.names = TRUE
    )

    if(
      length(package_file) == 0
    ) {

      missing_files <- c(
        missing_files,
        required_file
      )

    }

  }

  if(
    validate &&
    length(missing_files) > 0
  ) {

    stop(
      paste(
        "Bundle is incomplete.\n\nMissing files:\n",
        paste(
          missing_files,
          collapse = "\n"
        )
      )
    )

  }

  installed <- character()

  for(
    i in seq_len(
      nrow(manifest)
    )
  ) {

    required_file <- manifest$Required_File[i]

    pattern <- paste0(
      "^",
      gsub(
        "\\*",
        ".*",
        required_file
      ),
      "$"
    )

    package_file <- list.files(
      bundle_path,
      pattern = pattern,
      full.names = TRUE
    )

    if(
      length(package_file) == 0
    ) {

      next

    }

    utils::install.packages(
      package_file[1],
      repos = NULL,
      type = "source",
      lib = library_path
    )

    installed <- c(
      installed,
      basename(
        package_file[1]
      )
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

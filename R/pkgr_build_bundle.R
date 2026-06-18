#' Build Deployment Bundle
#'
#' Creates a deployment bundle containing
#' required package files and deployment manifest.
#'
#' @param package Package name.
#' @param repo_path Package repository path.
#' @param bundle_path Bundle output path.
#'
#' @return Bundle path.
#' @export

pkgr_build_bundle <- function(
    package,
    repo_path,
    bundle_path
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
    missing(bundle_path)
  ) {

    stop(
      "bundle_path is required."
    )

  }

  if(
    !dir.exists(bundle_path)
  ) {

    dir.create(
      bundle_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  bundle <- pkgr_bundle(
    package = package,
    repo_path = repo_path
  )

  pkgr_manifest(
    package = package,
    repo_path = repo_path,
    output_file = file.path(
      bundle_path,
      paste0(
        package,
        "_manifest.csv"
      )
    )
  )

  repo_files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  copied_count <- 0

  for(req_file in bundle$Required_Files) {

    pattern <- paste0(
      "^",
      gsub(
        "\\*",
        ".*",
        req_file
      ),
      "$"
    )

    match_file <- repo_files[
      grepl(
        pattern,
        basename(
          repo_files
        )
      )
    ]

    if(
      length(match_file) > 0
    ) {

      file.copy(
        from = match_file[1],
        to = bundle_path,
        overwrite = TRUE
      )

      copied_count <- copied_count + 1

    } else {

      warning(
        paste(
          "Required file not found:",
          req_file
        )
      )

    }

  }

  message(
    copied_count,
    " package files copied."
  )

  message(
    "Bundle created: ",
    normalizePath(
      bundle_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

  invisible(
    normalizePath(
      bundle_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

}

#' Repository Status
#'
#' Returns repository readiness status
#' for a package.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return List.
#' @export

pkgr_repo_status <- function(
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

  bundle <- pkgr_bundle(
    package = package,
    repo_path = repo_path
  )

  repo_files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$"
  )

  required <- bundle$Required_Files

  present <- character()

  missing <- character()

  for(req_file in required) {

    pattern <- paste0(
      "^",
      gsub(
        "\\*",
        ".*",
        req_file
      ),
      "$"
    )

    found <- any(
      grepl(
        pattern,
        repo_files
      )
    )

    if(found) {

      present <- c(
        present,
        req_file
      )

    } else {

      missing <- c(
        missing,
        req_file
      )

    }

  }

  status <- if(
    length(missing) == 0
  ) {

    "COMPLETE"

  } else {

    "INCOMPLETE"

  }

  list(
    Package = package,
    Required_Files = length(required),
    Present_Files = length(present),
    Missing_Files = length(missing),
    Status = status,
    Missing = missing
  )

}

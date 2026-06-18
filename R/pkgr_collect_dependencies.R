#' Collect Dependency Packages
#'
#' Copies required dependency package files
#' into a repository.
#'
#' @param package Package name.
#' @param source_path Source package folder.
#' @param repo_path Repository path.
#'
#' @return Data frame.
#' @export

pkgr_collect_dependencies <- function(
    package,
    source_path,
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
    missing(source_path)
  ) {

    stop(
      "source_path is required."
    )

  }

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  required <- pkgr_required_files(
    package
  )

  source_files <- list.files(
    source_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  copied <- data.frame(
    Package = character(),
    File = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )

  for(
    i in seq_len(
      nrow(required)
    )
  ) {

    req_file <- required$RequiredFile[i]

    match_file <- source_files[
      basename(source_files) ==
        req_file
    ]

    if(
      length(match_file) == 0
    ) {

      copied <- rbind(
        copied,
        data.frame(
          Package =
            required$Package[i],
          File =
            req_file,
          Status =
            "NOT FOUND",
          stringsAsFactors = FALSE
        )
      )

      next

    }

    file.copy(
      from = match_file[1],
      to = repo_path,
      overwrite = TRUE
    )

    copied <- rbind(
      copied,
      data.frame(
        Package =
          required$Package[i],
        File =
          req_file,
        Status =
          "COPIED",
        stringsAsFactors = FALSE
      )
    )

  }

  copied

}

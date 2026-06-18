#' Repository Cleanup
#'
#' Removes older package versions
#' and keeps latest version only.
#'
#' @param repo_path Repository path.
#'
#' @return Data frame.
#' @export

pkgr_repository_cleanup <- function(
    repo_path
) {

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  if(
    length(files) == 0
  ) {

    stop(
      "No package files found."
    )

  }

  info <- data.frame(
    File = basename(files),
    Package = sub(
      "_.*$",
      "",
      basename(files)
    ),
    stringsAsFactors = FALSE
  )

  removed <- character()

  for(pkg in unique(info$Package)) {

    pkg_files <- files[
      grepl(
        paste0("^", pkg, "_"),
        basename(files)
      )
    ]

    if(
      length(pkg_files) <= 1
    ) {

      next

    }

    versions <- sub(
      paste0("^", pkg, "_"),
      "",
      basename(pkg_files)
    )

    versions <- sub(
      "\\.tar\\.gz$",
      "",
      versions
    )

    latest <- which.max(
      package_version(
        versions
      )
    )

    remove_files <- pkg_files[
      -latest
    ]

    file.remove(
      remove_files
    )

    removed <- c(
      removed,
      basename(remove_files)
    )

  }

  data.frame(
    Removed = removed,
    stringsAsFactors = FALSE
  )

}

#' Synchronize Repository
#'
#' Copies missing dependency packages
#' from a source repository.
#'
#' @param package Package name.
#' @param source_repo Source repository.
#' @param target_repo Target repository.
#'
#' @return Data frame.
#' @export

pkgr_sync_repository <- function(
    package,
    source_repo,
    target_repo
) {

  if(missing(package)) {
    stop("package is required.")
  }

  if(missing(source_repo)) {
    stop("source_repo is required.")
  }

  if(missing(target_repo)) {
    stop("target_repo is required.")
  }

  status <- pkgr_repo_status(
    package = package,
    repo_path = target_repo
  )

  if(
    status$Missing_Files == 0
  ) {

    message(
      "Repository already complete."
    )

    return(
      invisible(NULL)
    )

  }

  source_files <- list.files(
    source_repo,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  results <- data.frame(
    File = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )

  for(req in status$Missing) {

    pkg <- sub(
      "_\\*\\.tar\\.gz$",
      "",
      req
    )

    match <- source_files[
      grepl(
        paste0("^", pkg, "_"),
        basename(source_files)
      )
    ]

    if(length(match) == 0) {

      results <- rbind(
        results,
        data.frame(
          File = req,
          Status = "NOT FOUND",
          stringsAsFactors = FALSE
        )
      )

      next

    }

    file.copy(
      match[1],
      target_repo,
      overwrite = TRUE
    )

    results <- rbind(
      results,
      data.frame(
        File = basename(match[1]),
        Status = "COPIED",
        stringsAsFactors = FALSE
      )
    )

  }

  results

}

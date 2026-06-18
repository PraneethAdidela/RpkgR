#' Import Package Files
#'
#' Imports package files into a repository.
#'
#' @param source_path Source folder.
#' @param repo_path Repository path.
#'
#' @return Data frame.
#' @export

pkgr_import_package <- function(
    source_path,
    repo_path
) {

  if(missing(source_path)) {
    stop("source_path is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  files <- list.files(
    source_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  if(length(files) == 0) {

    stop(
      "No package files found."
    )

  }

  copied <- file.copy(
    from = files,
    to = repo_path,
    overwrite = TRUE
  )

  data.frame(
    File = basename(files),
    Imported = copied,
    stringsAsFactors = FALSE
  )

}

#' Add Package(s) to Repository
#'
#' Copies package files into a repository and
#' automatically updates repository metadata.
#'
#' @param package_files Character vector of package files.
#' @param repo_path Repository path.
#'
#' @return Repository path.
#' @export

pkgr_add_package <- function(
    package_files,
    repo_path
) {

  if(missing(package_files)) {
    stop("package_files is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  contrib_path <- file.path(
    repo_path,
    "src",
    "contrib"
  )

  if(!dir.exists(contrib_path)) {

    stop(
      paste(
        "Repository does not exist:",
        contrib_path
      )
    )

  }

  package_files <- normalizePath(
    package_files,
    mustWork = TRUE
  )

  copied <- file.copy(
    from = package_files,
    to = contrib_path,
    overwrite = TRUE
  )

  if(any(!copied)) {

    warning(
      "Some package files were not copied."
    )

  }

  tools::write_PACKAGES(
    dir = contrib_path,
    type = "source"
  )

  message(
    "Repository updated successfully."
  )

  invisible(
    normalizePath(
      contrib_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

}

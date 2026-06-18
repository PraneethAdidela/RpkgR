#' Dependency Check
#'
#' Checks whether dependency files
#' exist in a package repository.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return Data frame.
#' @export

pkgr_dependency_check <- function(
    package,
    repo_path
) {

  if(missing(package)) {
    stop("package is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  deps <- pkgr_required_files(
    package
  )

  repo_files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$"
  )

  deps$Present <- ifelse(
    deps$RequiredFile %in% repo_files,
    "YES",
    "NO"
  )

  deps

}

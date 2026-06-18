#' Dependency Gap
#'
#' Returns missing dependency files.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return Data frame.
#' @export

pkgr_dependency_gap <- function(
    package,
    repo_path
) {

  if(missing(package)) {
    stop("package is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  deps <- pkgr_dependency_check(
    package = package,
    repo_path = repo_path
  )

  deps[
    deps$Present == "NO",
    c(
      "Package",
      "Version",
      "RequiredFile"
    ),
    drop = FALSE
  ]

}

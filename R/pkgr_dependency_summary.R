#' Dependency Summary
#'
#' Summarizes dependency availability.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' pkgr_dependency_summary(
#'   package = "Tplyr",
#'   repo_path = tempdir()
#' )
#' }
#'
#' @export

pkgr_dependency_summary <- function(
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

  present <- sum(
    deps$Present == "YES"
  )

  missing <- sum(
    deps$Present == "NO"
  )

  list(
    Package = package,
    DependenciesNeeded = nrow(deps),
    DependenciesPresent = present,
    DependenciesMissing = missing,
    RepositoryStatus = if(
      missing == 0
    ) {
      "COMPLETE"
    } else {
      "INCOMPLETE"
    },
    MissingPackages =
      deps$Package[
        deps$Present == "NO"
      ]
  )

}

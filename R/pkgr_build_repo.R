#' Build Repository
#'
#' Creates and populates a local CRAN-like repository.
#'
#' @param package_files Character vector of package files.
#' @param repo_path Repository path.
#'
#' @return Repository path.
#' @export

pkgr_build_repo <- function(
    package_files,
    repo_path
) {

  if(
    missing(package_files)
  ) {

    stop(
      "package_files is required."
    )

  }

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  pkgr_create_repo(
    repo_path = repo_path
  )

  pkgr_add_package(
    package_files = package_files,
    repo_path = repo_path
  )

  message(
    "Repository build completed."
  )

  invisible(
    normalizePath(
      file.path(
        repo_path,
        "src",
        "contrib"
      ),
      winslash = "/",
      mustWork = FALSE
    )
  )

}

#' Create Local Repository
#'
#' Creates a CRAN-like repository structure.
#'
#' @param repo_path Repository path.
#'
#' @return Repository path.
#'
#' @examples
#' repo_path <- tempdir()
#' pkgr_create_repo(
#'   repo_path = repo_path
#' )
#'
#' @export

pkgr_create_repo <- function(
    repo_path
) {

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  if(
    !dir.exists(
      repo_path
    )
  ) {

    dir.create(
      repo_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  contrib_path <- file.path(
    repo_path,
    "src",
    "contrib"
  )

  if(
    !dir.exists(
      contrib_path
    )
  ) {

    dir.create(
      contrib_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  message(
    "Repository created: ",
    normalizePath(
      contrib_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

  invisible(
    normalizePath(
      contrib_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

}

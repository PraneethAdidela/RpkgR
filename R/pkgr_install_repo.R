#' Install Package from Repository
#'
#' Installs a package from a local CRAN-like repository.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#' @param lib Optional library path.
#'
#' @return Invisible NULL.
#' @export

pkgr_install_repo <- function(
    package,
    repo_path,
    lib = NULL
) {

  if(
    missing(package)
  ) {

    stop(
      "package is required."
    )

  }

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  repo_url <- paste0(
    "file:///",
    gsub(
      "\\\\",
      "/",
      normalizePath(
        repo_path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  )

  utils::install.packages(
    pkgs = package,
    repos = repo_url,
    type = "source",
    lib = lib
  )

  invisible(
    NULL
  )

}

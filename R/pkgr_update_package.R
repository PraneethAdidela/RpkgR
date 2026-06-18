#' Update Package in Repository
#'
#' Removes older versions of a package
#' and adds the new version.
#'
#' @param package_file Package file.
#' @param repo_path Repository path.
#'
#' @return List.
#' @export

pkgr_update_package <- function(
    package_file,
    repo_path
) {

  if(missing(package_file)) {
    stop("package_file is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  if(!file.exists(package_file)) {
    stop("package_file does not exist.")
  }

  package_file <- normalizePath(
    package_file,
    winslash = "/"
  )

  repo_path <- normalizePath(
    repo_path,
    winslash = "/"
  )

  pkg_name <- sub(
    "_.*$",
    "",
    basename(package_file)
  )

  destination_file <- file.path(
    repo_path,
    basename(package_file)
  )

  if(
    normalizePath(
      dirname(package_file),
      winslash = "/"
    ) == repo_path
  ) {

    return(
      list(
        Package = pkg_name,
        Removed = character(),
        Added = basename(package_file),
        Status = "ALREADY_IN_REPOSITORY"
      )
    )

  }

  existing_files <- list.files(
    repo_path,
    pattern = paste0(
      "^",
      pkg_name,
      "_.*\\.tar\\.gz$"
    ),
    full.names = TRUE
  )

  removed <- character()

  if(length(existing_files) > 0) {

    removed <- basename(existing_files)

    file.remove(existing_files)

  }

  file.copy(
    from = package_file,
    to = destination_file,
    overwrite = TRUE
  )

  list(
    Package = pkg_name,
    Removed = removed,
    Added = basename(package_file),
    Status = "UPDATED"
  )

}

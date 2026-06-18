#' Resolve Package Version
#'
#' Selects the latest available version
#' that satisfies a minimum version.
#'
#' @param package Package name.
#' @param min_version Minimum required version.
#' @param repo_path Repository path.
#'
#' @return Character string.
#' @export

pkgr_resolve_version <- function(
    package,
    min_version,
    repo_path
) {

  repo_files <- list.files(
    path = repo_path,
    pattern = "\\.(tar\\.gz|zip)$",
    full.names = FALSE
  )

  pkg_files <- repo_files[
    grepl(
      paste0("^", package, "_"),
      repo_files
    )
  ]

  if(length(pkg_files) == 0) {

    return(NA_character_)

  }

  versions <- sub(
    paste0("^", package, "_"),
    "",
    pkg_files
  )

  versions <- sub(
    "\\.(tar\\.gz|zip)$",
    "",
    versions
  )

  versions <- versions[
    package_version(versions) >=
      package_version(min_version)
  ]

  if(length(versions) == 0) {

    return(NA_character_)

  }

  as.character(
    utils::tail(
      sort(
        package_version(versions)
      ),
      1
    )
  )

}

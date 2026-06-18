#' Dependency Manifest
#'
#' Creates a dependency manifest file
#' for package approval and deployment.
#'
#' @param package Package name.
#' @param output_file Output csv file.
#'
#' @return Data frame.
#' @export

pkgr_dependency_manifest <- function(
    package,
    output_file
) {

  if(
    missing(package)
  ) {

    stop(
      "package is required."
    )

  }

  if(
    missing(output_file)
  ) {

    stop(
      "output_file is required."
    )

  }

  deps <- pkgr_required_files(
    package
  )

  pkg_version <- as.character(
    utils::packageVersion(
      package
    )
  )

  manifest <- rbind(
    deps,
    data.frame(
      Package = package,
      Version = pkg_version,
      RequiredFile = paste0(
        package,
        "_",
        pkg_version,
        ".tar.gz"
      ),
      stringsAsFactors = FALSE
    )
  )

  utils::write.csv(
    manifest,
    output_file,
    row.names = FALSE
  )

  message(
    "Dependency manifest created: ",
    output_file
  )

  manifest

}

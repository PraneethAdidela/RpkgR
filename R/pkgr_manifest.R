#' Create Deployment Manifest
#'
#' Creates a CSV manifest for package deployment.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#' @param output_file Output CSV file.
#'
#' @return Data frame.
#' @export

pkgr_manifest <- function(
    package,
    repo_path,
    output_file
) {

  rec <- pkgr_recommend(
    package = package,
    repo_path = repo_path
  )

  manifest <- data.frame(
    Package = c(
      rec$Dependencies$Package,
      package
    ),
    Required_File = rec$Required_Files,
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    manifest,
    output_file,
    row.names = FALSE
  )

  message(
    "Manifest created: ",
    normalizePath(
      output_file,
      winslash = "/",
      mustWork = FALSE
    )
  )

  manifest

}

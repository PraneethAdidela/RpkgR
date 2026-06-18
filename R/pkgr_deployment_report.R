#' Deployment Report
#'
#' Creates deployment readiness report.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#' @param output_file Output csv file.
#'
#' @return Data frame.
#' @export

pkgr_deployment_report <- function(
    package,
    repo_path,
    output_file
) {

  if(missing(package)) {
    stop("package is required.")
  }

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  if(missing(output_file)) {
    stop("output_file is required.")
  }

  status <- pkgr_repo_status(
    package = package,
    repo_path = repo_path
  )

  deps <- pkgr_required_files(
    package
  )

  report <- data.frame(
    Package = deps$Package,
    Version = deps$Version,
    RequiredFile = deps$RequiredFile,
    Present = ifelse(
      deps$RequiredFile %in%
        list.files(
          repo_path,
          pattern = "\\.tar\\.gz$"
        ),
      "YES",
      "NO"
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    report,
    output_file,
    row.names = FALSE
  )

  message(
    "Deployment report created: ",
    output_file
  )

  attr(
    report,
    "RepositoryStatus"
  ) <- status$Status

  attr(
    report,
    "MissingFiles"
  ) <- status$Missing_Files

  report

}

#' Repository Report
#'
#' Creates repository governance report.
#'
#' @param repo_path Repository path.
#' @param output_file Output csv file.
#'
#' @return Data frame.
#' @export

pkgr_repository_report <- function(
    repo_path,
    output_file
) {

  if(missing(repo_path)) {
    stop("repo_path is required.")
  }

  if(missing(output_file)) {
    stop("output_file is required.")
  }

  inventory <- pkgr_export_repository(
    repo_path = repo_path,
    output_file = tempfile(
      fileext = ".csv"
    )
  )

  health <- pkgr_repository_health(
    repo_path
  )

  validation <- pkgr_validate_repository(
    repo_path
  )

  report <- data.frame(
    Package = character(),
    Version = character(),
    MissingDependencies = integer(),
    RepositoryStatus = character(),
    ValidationResult = character(),
    HealthScore = numeric(),
    stringsAsFactors = FALSE
  )

  for(i in seq_len(nrow(inventory))) {

    pkg <- inventory$Package[i]

    status <- pkgr_repo_status(
      package = pkg,
      repo_path = repo_path
    )

    report <- rbind(
      report,
      data.frame(
        Package = pkg,
        Version = inventory$Version[i],
        MissingDependencies =
          status$Missing_Files,
        RepositoryStatus =
          status$Status,
        ValidationResult =
          validation$OverallResult,
        HealthScore =
          health$HealthScore,
        stringsAsFactors = FALSE
      )
    )

  }

  utils::write.csv(
    report,
    output_file,
    row.names = FALSE
  )

  message(
    "Repository report created: ",
    output_file
  )

  report

}

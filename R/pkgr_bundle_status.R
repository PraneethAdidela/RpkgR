#' Bundle Status
#'
#' Summarizes bundle readiness.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#' @param bundle_path Bundle path.
#'
#' @return List.
#' @export

pkgr_bundle_status <- function(
    package,
    repo_path,
    bundle_path
) {

  validation <- pkgr_validate_bundle(
    package = package,
    repo_path = repo_path,
    bundle_path = bundle_path
  )

  present_count <- sum(
    validation$Present == "YES"
  )

  missing_count <- sum(
    validation$Present == "NO"
  )

  list(
    Package = package,
    Required_Files = nrow(validation),
    Present_Files = present_count,
    Missing_Files = missing_count,
    Status = ifelse(
      missing_count == 0,
      "READY",
      "INCOMPLETE"
    )
  )

}

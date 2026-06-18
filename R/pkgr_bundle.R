#' Create Deployment Bundle
#'
#' Generates deployment bundle information.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return List.
#' @export

pkgr_bundle <- function(
    package,
    repo_path
) {

  rec <- pkgr_recommend(
    package = package,
    repo_path = repo_path
  )

  list(
    Package = rec$Package,
    Recommended_Version =
      rec$Recommended_Version,
    Required_Files =
      rec$Required_Files,
    Installation_Order =
      rec$Installation_Order
  )

}

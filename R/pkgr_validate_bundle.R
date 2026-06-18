#' Validate Deployment Bundle
#'
#' Checks whether all required package files
#' exist in a directory.
#'
#' @param package Package name.
#' @param repo_path Package repository path.
#' @param bundle_path Location containing tar.gz files.
#'
#' @return Data frame.
#' @export

pkgr_validate_bundle <- function(
    package,
    repo_path,
    bundle_path
) {

  bundle <- pkgr_bundle(
    package = package,
    repo_path = repo_path
  )

  files_found <- list.files(
    bundle_path
  )

  out <- data.frame(
    Required_File =
      bundle$Required_Files,
    stringsAsFactors = FALSE
  )

  out$Present <- ifelse(
    sapply(
      out$Required_File,
      function(x) {

        pattern <- sub(
          "\\*",
          ".*",
          x
        )

        any(
          grepl(
            pattern,
            files_found
          )
        )

      }
    ),
    "YES",
    "NO"
  )

  out

}

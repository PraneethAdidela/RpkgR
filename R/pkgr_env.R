#' Display Environment Information
#'
#' Shows information about the current R environment.
#'
#' @return Data frame containing environment details.
#' @export

pkgr_env <- function() {

  env_info <- data.frame(
    Parameter = c(
      "R Version",
      "Platform"
    ),
    Value = c(
      as.character(getRversion()),
      R.version$platform
    ),
    stringsAsFactors = FALSE
  )

  lib_info <- data.frame(
    Parameter = rep("Library Path", length(.libPaths())),
    Value = .libPaths(),
    stringsAsFactors = FALSE
  )

  rbind(env_info, lib_info)

}

#' Required Package Files
#'
#' Returns package files required for installation.
#'
#' @param package Package name.
#'
#' @return Data frame.
#'
#' @examples
#' pkgr_required_files(
#'   package = "Tplyr"
#' )
#'
#' @export

pkgr_required_files <- function(
    package
) {

  deps <- pkgr_resolve_dependencies(
    package
  )

  deps <- deps[
    deps$BasePackage == "NO",
    ,
    drop = FALSE
  ]

  if (
    nrow(deps) == 0
  ) {

    pkg_version <- tryCatch(

      as.character(
        utils::packageVersion(
          package
        )
      ),

      error = function(e) {
        NA_character_
      }

    )

    return(
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

  }

  deps$RequiredFile <- paste0(
    deps$Package,
    "_",
    deps$Version,
    ".tar.gz"
  )

  deps[
    ,
    c(
      "Package",
      "Version",
      "RequiredFile"
    )
  ]

}

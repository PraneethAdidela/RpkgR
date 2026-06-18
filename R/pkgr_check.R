#' Check Package Status
#'
#' Check whether a package is installed.
#'
#' @param package Package name.
#'
#' @return Data frame containing package status.
#' @export

pkgr_check <- function(package) {

  ip <- utils::installed.packages()

  if(package %in% rownames(ip)) {

    data.frame(
      Package = package,
      Installed = "YES",
      Version = ip[package, "Version"],
      Library = ip[package, "LibPath"],
      stringsAsFactors = FALSE
    )

  } else {

    data.frame(
      Package = package,
      Installed = "NO",
      Version = NA,
      Library = NA,
      stringsAsFactors = FALSE
    )

  }

}

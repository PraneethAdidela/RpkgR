#' Package Dependency Tree
#'
#' Returns recursive dependency tree.
#'
#' @param package Package name.
#'
#' @return Data frame.
#' @export

pkgr_dependency_tree <- function(package) {

  if(!package %in% rownames(utils::installed.packages())) {

    stop(
      paste0(
        "Package '",
        package,
        "' is not installed."
      )
    )

  }

  deps <- tools::package_dependencies(
    packages = package,
    db = utils::installed.packages(),
    recursive = TRUE
  )

  deps <- sort(
    unique(
      unlist(deps)
    )
  )

  ip <- utils::installed.packages()

  out <- data.frame(
    Dependency = deps,
    Installed = ifelse(
      deps %in% rownames(ip),
      "YES",
      "NO"
    ),
    Version = ifelse(
      deps %in% rownames(ip),
      ip[deps, "Version"],
      NA
    ),
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL

  out

}

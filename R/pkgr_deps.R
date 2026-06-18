#' Get Package Dependencies
#'
#' Returns package dependencies and installation status.
#'
#' @param package Package name.
#' @param recursive Include dependency tree.
#'
#' @return Data frame containing dependency information.
#' @export

pkgr_deps <- function(
    package,
    recursive = FALSE
) {

  # Check package exists
  if (!package %in% rownames(utils::installed.packages())) {

    stop(
      paste0(
        "Package '",
        package,
        "' is not installed."
      )
    )

  }

  # Get dependencies
  deps <- tools::package_dependencies(
    packages = package,
    db = utils::installed.packages(),
    recursive = recursive
  )

  deps <- unique(unlist(deps))

  # No dependencies
  if (length(deps) == 0) {

    return(
      data.frame(
        Dependency = character(),
        Installed = character(),
        BasePackage = character(),
        Version = character(),
        stringsAsFactors = FALSE
      )
    )

  }

  deps <- sort(deps)

  # Installed packages
  ip <- utils::installed.packages()

  # Base and recommended packages
  base_pkgs <- rownames(
    utils::installed.packages(
      priority = c("base", "recommended")
    )
  )

  # Versions
  versions <- sapply(
    deps,
    function(x) {

      if (x %in% rownames(ip)) {

        ip[x, "Version"]

      } else {

        NA_character_

      }

    }
  )

  out <- data.frame(
    Dependency = deps,
    Installed = ifelse(
      deps %in% rownames(ip),
      "YES",
      "NO"
    ),
    BasePackage = ifelse(
      deps %in% base_pkgs,
      "YES",
      "NO"
    ),
    Version = versions,
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL

  out

}

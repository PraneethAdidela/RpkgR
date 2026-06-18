#' Resolve Dependencies Recursively
#'
#' Returns all dependencies and metadata.
#'
#' @param package Package name.
#' @param recursive Logical.
#'
#' @return Data frame.
#' @keywords internal

pkgr_resolve_dependencies <- function(
    package,
    recursive = TRUE
) {

  repos <- getOption("repos")

  if (
    is.null(repos["CRAN"]) ||
    repos["CRAN"] == "@CRAN@"
  ) {

    options(
      repos = c(
        CRAN = "https://cloud.r-project.org"
      )
    )

  }

  deps <- tools::package_dependencies(
    packages = package,
    recursive = recursive
  )

  dep_names <- unique(
    unlist(deps)
  )

  if (
    length(dep_names) == 0
  ) {

    return(
      data.frame(
        Package = character(),
        Installed = character(),
        Version = character(),
        BasePackage = character(),
        stringsAsFactors = FALSE
      )
    )

  }

  ip <- utils::installed.packages(
    lib.loc = .libPaths()
  )

  installed <- rownames(ip)

  out <- data.frame(
    Package = sort(dep_names),
    stringsAsFactors = FALSE
  )

  out$Installed <- ifelse(
    out$Package %in% installed,
    "YES",
    "NO"
  )

  out$Version <- sapply(
    out$Package,
    function(pkg) {

      if (
        pkg %in% installed
      ) {

        as.character(
          ip[pkg, "Version"]
        )

      } else {

        NA_character_

      }

    }
  )

  base_pkgs <- rownames(
    utils::installed.packages(
      priority = "base"
    )
  )

  out$BasePackage <- ifelse(
    out$Package %in% base_pkgs,
    "YES",
    "NO"
  )

  out

}

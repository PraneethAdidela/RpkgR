#' Build Source Packages
#'
#' Creates source package files for
#' installed packages.
#'
#' @param packages Character vector.
#' @param output_path Output folder.
#'
#' @return Data frame.
#' @export

pkgr_build_sources <- function(
    packages,
    output_path
) {

  if(
    missing(packages)
  ) {

    stop(
      "packages is required."
    )

  }

  if(
    missing(output_path)
  ) {

    stop(
      "output_path is required."
    )

  }

  if(
    !dir.exists(output_path)
  ) {

    dir.create(
      output_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  ip <- utils::installed.packages()

  results <- data.frame(
    Package = character(),
    Version = character(),
    Status = character(),
    stringsAsFactors = FALSE
  )

  for(pkg in packages) {

    if(
      !(pkg %in% rownames(ip))
    ) {

      results <- rbind(
        results,
        data.frame(
          Package = pkg,
          Version = NA_character_,
          Status = "NOT INSTALLED",
          stringsAsFactors = FALSE
        )
      )

      next

    }

    ver <- ip[
      pkg,
      "Version"
    ]

    results <- rbind(
      results,
      data.frame(
        Package = pkg,
        Version = ver,
        Status = "AVAILABLE",
        stringsAsFactors = FALSE
      )
    )

  }

  results

}

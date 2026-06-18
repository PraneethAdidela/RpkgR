#' Parse Dependency Versions
#'
#' Extract package names and minimum versions.
#'
#' @param x Dependency string.
#'
#' @return Data frame.
#' @keywords internal

pkgr_parse_version_deps <- function(x) {

  if(is.na(x) || length(x) == 0) {

    return(
      data.frame(
        Package = character(),
        MinVersion = character(),
        stringsAsFactors = FALSE
      )
    )

  }

  deps <- unlist(
    strsplit(x, ",")
  )

  deps <- trimws(deps)

  pkg <- character()
  ver <- character()

  for(d in deps) {

    pkg_name <- trimws(
      gsub(
        "\\(.*\\)",
        "",
        d
      )
    )

    version_match <- regmatches(
      d,
      regexpr(
        "[0-9]+\\.[0-9]+(\\.[0-9]+)?",
        d
      )
    )

    if(length(version_match) == 0 ||
       version_match == "") {

      version_match <- NA_character_

    }

    pkg <- c(pkg, pkg_name)
    ver <- c(ver, version_match)

  }

  out <- data.frame(
    Package = pkg,
    MinVersion = ver,
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL

  out

}

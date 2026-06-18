#' Parse Dependency String
#'
#' @param x Dependency string.
#'
#' @return Character vector.
#' @keywords internal

pkgr_parse_deps <- function(x) {

  if(is.na(x) || length(x) == 0) {

    return(character())

  }

  deps <- unlist(
    strsplit(x, ",")
  )

  deps <- trimws(deps)

  deps <- gsub(
    "\\(.*\\)",
    "",
    deps
  )

  deps <- trimws(deps)

  deps[deps != ""]

}

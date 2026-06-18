#' Extract Required R Version
#'
#' @param depends Depends field from DESCRIPTION
#'
#' @return Required R version.
#' @keywords internal

pkgr_r_required <- function(depends) {

  if (is.na(depends)) {

    return(NA_character_)

  }

  match <- regmatches(
    depends,
    regexpr(
      "R \\(>= [0-9.]+\\)",
      depends
    )
  )

  if (length(match) == 0 || match == "") {

    return(NA_character_)

  }

  gsub(
    "R \\(>= |\\)",
    "",
    match
  )

}

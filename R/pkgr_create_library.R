#' Create Local Library
#'
#' Creates a local package library.
#'
#' @param library_path Library path.
#'
#' @return Library path.
#' @export

pkgr_create_library <- function(
    library_path
) {

  if(
    missing(library_path)
  ) {

    stop(
      "library_path is required."
    )

  }

  if(
    !dir.exists(
      library_path
    )
  ) {

    dir.create(
      library_path,
      recursive = TRUE,
      showWarnings = FALSE
    )

  }

  message(
    "Library created: ",
    normalizePath(
      library_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

  invisible(
    normalizePath(
      library_path,
      winslash = "/",
      mustWork = FALSE
    )
  )

}

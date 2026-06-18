#' Use Local Library
#'
#' Adds a local library to .libPaths().
#'
#' @param library_path Library path.
#'
#' @return Current library paths.
#' @export

pkgr_use_library <- function(
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
    !dir.exists(library_path)
  ) {

    stop(
      paste(
        "Library does not exist:",
        library_path
      )
    )

  }

  .libPaths(
    c(
      normalizePath(
        library_path,
        winslash = "/",
        mustWork = TRUE
      ),
      .libPaths()
    )
  )

  message(
    "Library activated: ",
    normalizePath(
      library_path,
      winslash = "/",
      mustWork = TRUE
    )
  )

  .libPaths()

}

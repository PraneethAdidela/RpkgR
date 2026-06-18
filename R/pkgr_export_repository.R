#' Export Repository Inventory
#'
#' Exports repository package inventory.
#'
#' @param repo_path Repository path.
#' @param output_file Output CSV file.
#'
#' @return Data frame.
#' @export

pkgr_export_repository <- function(
    repo_path,
    output_file
) {

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  if(
    missing(output_file)
  ) {

    stop(
      "output_file is required."
    )

  }

  repo_files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$",
    full.names = FALSE
  )

  if(
    length(repo_files) == 0
  ) {

    stop(
      "No package files found."
    )

  }

  inventory <- data.frame(
    File = repo_files,
    stringsAsFactors = FALSE
  )

  inventory$Package <- sub(
    "_[0-9].*$",
    "",
    inventory$File
  )

  inventory$Version <- sub(
    "^.*_",
    "",
    inventory$File
  )

  inventory$Version <- sub(
    "\\.tar\\.gz$",
    "",
    inventory$Version
  )

  inventory <- inventory[
    ,
    c(
      "Package",
      "Version",
      "File"
    )
  ]

  utils::write.csv(
    inventory,
    output_file,
    row.names = FALSE
  )

  message(
    "Repository inventory exported: ",
    output_file
  )

  inventory

}

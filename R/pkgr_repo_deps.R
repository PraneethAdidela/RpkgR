#' Read Package Metadata
#'
#' Read DESCRIPTION information from a package source archive.
#'
#' @param package_file Path to tar.gz package.
#'
#' @return List containing package metadata.
#' @export

pkgr_repo_deps <- function(package_file) {

  temp_dir <- tempfile()

  dir.create(temp_dir)

  utils::untar(
    package_file,
    exdir = temp_dir
  )

  desc_file <- list.files(
    temp_dir,
    pattern = "^DESCRIPTION$",
    recursive = TRUE,
    full.names = TRUE
  )[1]

  if(is.na(desc_file)) {

    stop("DESCRIPTION file not found.")

  }

  desc <- read.dcf(desc_file)

  list(
    Package = desc[1, "Package"],
    Version = desc[1, "Version"],
    Depends = if("Depends" %in% colnames(desc))
      desc[1, "Depends"] else NA,
    Imports = if("Imports" %in% colnames(desc))
      desc[1, "Imports"] else NA,
    Suggests = if("Suggests" %in% colnames(desc))
      desc[1, "Suggests"] else NA
  )

}

#' Repository Snapshot
#'
#' Creates repository audit snapshot.
#'
#' @param repo_path Repository path.
#' @param output_file Output csv file.
#'
#' @return Data frame.
#' @export

pkgr_repository_snapshot <- function(
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

  inventory <- pkgr_export_repository(
    repo_path = repo_path,
    output_file = tempfile(
      fileext = ".csv"
    )
  )

  snapshot <- data.frame(
    SnapshotDate = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    Package = inventory$Package,
    Version = inventory$Version,
    File = inventory$File,
    stringsAsFactors = FALSE
  )

  utils::write.csv(
    snapshot,
    output_file,
    row.names = FALSE
  )

  message(
    "Repository snapshot created: ",
    output_file
  )

  snapshot

}

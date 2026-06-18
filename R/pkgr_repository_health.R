#' Repository Health
#'
#' Evaluates repository completeness.
#'
#' @param repo_path Repository path.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' pkgr_repository_health(
#'   repo_path = tempdir()
#' )
#' }
#'
#' @export

pkgr_repository_health <- function(
    repo_path
) {

  if(
    missing(repo_path)
  ) {

    stop(
      "repo_path is required."
    )

  }

  files <- list.files(
    repo_path,
    pattern = "\\.tar\\.gz$",
    full.names = TRUE
  )

  if(
    length(files) == 0
  ) {

    stop(
      "Repository contains no package files."
    )

  }

  inventory <- pkgr_export_repository(
    repo_path = repo_path,
    output_file = tempfile(
      fileext = ".csv"
    )
  )

  issues <- list()

  total_required <- 0

  total_missing <- 0

  for(i in seq_len(nrow(inventory))) {

    pkg <- inventory$Package[i]

    status <- tryCatch(
      pkgr_repo_status(
        package = pkg,
        repo_path = repo_path
      ),
      error = function(e) NULL
    )

    if(
      !is.null(status)
    ) {

      total_required <-
        total_required +
        status$Required_Files

      total_missing <-
        total_missing +
        status$Missing_Files

      issues[[pkg]] <- status

    }

  }

  score <- if(
    total_required == 0
  ) {

    100

  } else {

    round(
      100 *
        (
          total_required -
            total_missing
        ) /
        total_required,
      1
    )

  }

  health <- if(
    score == 100
  ) {

    "HEALTHY"

  } else if(
    score >= 75
  ) {

    "WARNING"

  } else {

    "CRITICAL"

  }

  list(
    Repository = repo_path,
    Packages = nrow(inventory),
    Files = length(files),
    Health = health,
    HealthScore = score,
    MissingDependencies =
      total_missing,
    Inventory = inventory
  )

}

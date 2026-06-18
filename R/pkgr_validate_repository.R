#' Validate Repository
#'
#' Performs repository validation checks.
#'
#' @param repo_path Repository path.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' pkgr_validate_repository(
#'   repo_path = tempdir()
#' )
#' }
#'
#' @export

pkgr_validate_repository <- function(
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
    pattern = "\\.tar\\.gz$"
  )

  empty_repo <- length(files) == 0

  inventory <- tryCatch(
    pkgr_export_repository(
      repo_path = repo_path,
      output_file = tempfile(
        fileext = ".csv"
      )
    ),
    error = function(e) NULL
  )

  duplicate_packages <- FALSE

  if(
    !is.null(inventory)
  ) {

    duplicate_packages <-
      any(
        duplicated(
          inventory$Package
        )
      )

  }

  dependency_issues <- FALSE

  if(
    !is.null(inventory)
  ) {

    for(
      pkg in inventory$Package
    ) {

      status <- pkgr_repo_status(
        package = pkg,
        repo_path = repo_path
      )

      if(
        status$Missing_Files > 0
      ) {

        dependency_issues <- TRUE
        break

      }

    }

  }

  invalid_files <- any(
    !grepl(
      "^[A-Za-z0-9\\.]+_[0-9].*\\.tar\\.gz$",
      files
    )
  )

  checks <- data.frame(
    Check = c(
      "Empty Repository",
      "Duplicate Packages",
      "Dependency Issues",
      "Invalid Files"
    ),
    Result = c(
      ifelse(
        empty_repo,
        "FAIL",
        "PASS"
      ),
      ifelse(
        duplicate_packages,
        "FAIL",
        "PASS"
      ),
      ifelse(
        dependency_issues,
        "FAIL",
        "PASS"
      ),
      ifelse(
        invalid_files,
        "FAIL",
        "PASS"
      )
    ),
    stringsAsFactors = FALSE
  )

  overall <- if(
    any(
      checks$Result == "FAIL"
    )
  ) {

    "FAIL"

  } else {

    "PASS"

  }

  list(
    Repository = repo_path,
    Validation = checks,
    OverallResult = overall
  )

}

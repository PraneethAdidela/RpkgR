#' Recommend Package Version
#'
#' Generates package recommendation report.
#'
#' @param package Package name.
#' @param repo_path Repository path.
#'
#' @return List containing recommendation report.
#' @export

pkgr_recommend <- function(
    package,
    repo_path
) {

  current_r <- as.character(
    getRversion()
  )

  repo_files <- list.files(
    path = repo_path,
    pattern = "\\.(tar\\.gz|zip)$",
    full.names = TRUE
  )

  if(length(repo_files) == 0) {

    stop(
      paste0(
        "Repository path contains no package files: ",
        repo_path
      )
    )

  }

  pkg_files <- repo_files[
    grepl(
      paste0("^", package, "_"),
      basename(repo_files)
    )
  ]

  if(length(pkg_files) == 0) {

    stop(
      paste0(
        "No versions of ",
        package,
        " found."
      )
    )

  }

  versions <- character()

  compatible <- character()

  incompatible <- character()

  dep_table <- data.frame()

  for(file in pkg_files) {

    meta <- tryCatch(
      pkgr_repo_deps(file),
      error = function(e) NULL
    )

    if(is.null(meta)) {
      next
    }

    ver <- as.character(
      meta$Version
    )

    req_r <- pkgr_r_required(
      meta$Depends
    )

    versions <- c(
      versions,
      ver
    )

    if(is.na(req_r)) {

      compatible <- c(
        compatible,
        ver
      )

    } else {

      if(
        package_version(current_r) >=
        package_version(req_r)
      ) {

        compatible <- c(
          compatible,
          ver
        )

      } else {

        incompatible <- c(
          incompatible,
          paste0(
            ver,
            " (Requires R >= ",
            req_r,
            ")"
          )
        )

      }

    }

    dep_table <- pkgr_parse_version_deps(
      meta$Imports
    )

  }

  recommended <- if(
    length(compatible) > 0
  ) {

    as.character(
      utils::tail(
        sort(
          package_version(
            compatible
          )
        ),
        1
      )
    )

  } else {

    NA_character_

  }

  installed_pkgs <- rownames(
    utils::installed.packages()
  )

  core_pkgs <- rownames(
    utils::installed.packages(
      priority = c(
        "base",
        "recommended"
      )
    )
  )

  if(nrow(dep_table) > 0) {

    dep_table <- dep_table[
      !(dep_table$Package %in% core_pkgs),
      ,
      drop = FALSE
    ]

    dep_table$Installed <- ifelse(
      dep_table$Package %in%
        installed_pkgs,
      "YES",
      "NO"
    )

    dep_table$InstalledVersion <- sapply(
      dep_table$Package,
      function(pkg) {

        if(pkg %in% installed_pkgs) {

          as.character(
            utils::packageVersion(pkg)
          )

        } else {

          NA_character_

        }

      }
    )

    dep_table$RequirementMet <- mapply(
      function(
    inst_ver,
    min_ver
      ) {

        if(is.na(min_ver)) {
          return("YES")
        }

        if(is.na(inst_ver)) {
          return("NO")
        }

        if(
          package_version(inst_ver) >=
          package_version(min_ver)
        ) {

          "YES"

        } else {

          "NO"

        }

      },
    dep_table$InstalledVersion,
    dep_table$MinVersion
    )

  }

  recursive_deps <- pkgr_resolve_dependencies(
    package
  )

  recursive_deps <- recursive_deps[
    recursive_deps$BasePackage == "NO",
    ,
    drop = FALSE
  ]

  required_files <- character()

  if(nrow(dep_table) > 0) {

    required_files <- paste0(
      dep_table$Package,
      "_*.tar.gz"
    )

  }

  required_files <- c(
    required_files,
    paste0(
      package,
      "_",
      recommended,
      ".tar.gz"
    )
  )

  installation_order <- character()

  if(nrow(dep_table) > 0) {

    installation_order <- dep_table$Package

  }

  installation_order <- c(
    installation_order,
    package
  )

  list(
    Package = package,
    Current_R_Version = current_r,
    Available_Versions = sort(
      unique(
        versions
      )
    ),
    Compatible_Versions = compatible,
    Incompatible_Versions = incompatible,
    Recommended_Version = recommended,
    Dependencies = dep_table,
    Recursive_Dependencies = recursive_deps,
    Required_Files = required_files,
    Installation_Order = installation_order
  )

}

test_that(
  "pkgr_repository_health handles empty repository",
  {

    repo <- tempfile()

    dir.create(
      repo,
      recursive = TRUE
    )

    expect_error(
      pkgr_repository_health(
        repo
      )
    )

  }
)

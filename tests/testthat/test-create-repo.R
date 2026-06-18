test_that(
  "pkgr_create_repo creates repository",
  {

    repo <- tempfile()

    pkgr_create_repo(
      repo_path = repo
    )

    expect_true(
      dir.exists(repo)
    )

  }
)

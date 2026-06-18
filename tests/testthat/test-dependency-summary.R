test_that(
  "pkgr_dependency_summary returns summary",
  {

    result <- pkgr_dependency_summary(
      package = "stats",
      repo_path =
        "C:/Users/320120844/Desktop/Package/RpkgR"
    )

    expect_true(
      is.list(result)
    )

    expect_equal(
      result$RepositoryStatus,
      "INCOMPLETE"
    )

  }
)

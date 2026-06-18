test_that(
  "pkgr_required_files returns dependencies",
  {

    deps <- pkgr_required_files(
      "stats"
    )

    expect_gt(
      nrow(deps),
      0
    )

  }
)

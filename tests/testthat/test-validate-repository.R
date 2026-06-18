test_that(
  "pkgr_validate_repository detects dependency issues",
  {

    result <- pkgr_validate_repository(
      "C:/Users/320120844/Desktop/Package/RpkgR"
    )

    expect_equal(
      result$OverallResult,
      "FAIL"
    )

  }
)

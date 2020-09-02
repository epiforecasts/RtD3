context("readInEpiNow2")

testthat::test_that("Test that readInEpiNow2 runs without error in dry run mode when used with summaryWidget", {
  testthat::skip_on_cran()

  out <- list("Cases" = readInEpiNow2(
    path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary",
    region_var = "country"))

  testthat::expect_true(summaryWidget(rtData = out,
                                      dryRun = TRUE))

})



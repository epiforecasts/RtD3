context("joinRtData")

testthat::test_that("Test that joinRtData runs without error in dry run mode when used with summaryWidget", {
  testthat::skip_on_cran()

  subnational <-   national <- list("Cases" = readInEpiNow2(
    path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/subnational/italy/cases/summary",
    region_var = "region"))

  national <- list("Cases" = readInEpiNow2(
    path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary",
    region_var = "country"),
    regions = "Italy")

  out <- list()
  out$Cases <- joinRtData(subnational$Cases, national$Cases)


  testthat::expect_true(summaryWidget(rtData = out,
                                      dryRun = TRUE))

})

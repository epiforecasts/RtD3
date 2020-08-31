

context("readInEpiNow2")

test_that("Test that readInEpiNow2 runs without error in dry run mode when used with summaryWidget", {
  skip_on_cran()

  out <- list("Cases" = readInEpiNow2(
    path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary",
    region_var = "country"))


  summaryData <- rtData$Cases$summary
  geoData <- rnaturalearth::ne_countries(returnclass = 'sf')

  out$Cases <- out$Cases[-1]

  testthat::expect_true(suppressWarnings(summaryWidget(geoData = geoData,
                                      summaryData = summaryData,
                                      rtData = out,
                                      dryRun = TRUE)))

})


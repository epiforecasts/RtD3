testthat::test_that('jsonNull works as expected', {

  d <- data.frame('a' = c(1, 2, 3), 'b' = c(1, 2, 3))

  testthat::expect_identical(class(jsonNull(d)), 'json')

  testthat::expect_identical(class(jsonNull(NULL)), 'NULL')

})

testthat::test_that('geojsonNull works as expected', {

  d <- sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))

  testthat::expect_identical(class(geojsonNull(d)), c('geojson', 'json'))

  testthat::expect_identical(class(geojsonNull(NULL)), 'NULL')

})

testthat::test_that('summaryWidget dryrun works as expected', {

  testthat::expect_equal(summaryWidget(dryRun = T), T)

})




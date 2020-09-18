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

testthat::test_that('check_geoData_names works as expected', {

  gd1 <- data.frame(sovereignt = c('a', 'b', 'c', 'd'))
  gd2 <- data.frame(sovereignt = c('a', 'b', 'c', 'd', 'e', 'f'))

  rd1 <- list('a'=list('a'=NULL, 'b'=data.frame(region = c('z', 'y', 'x', 'w')), 'c' = NULL, 'd' = NULL))
  rd2 <- list('a'=list('a'=NULL, 'b'=data.frame(region = c('z', 'y', 'x', 'w', 'v', 'u')), 'c' = NULL, 'd' = NULL))

  testthat::expect_warning(check_geoData_names(gd1, rd1))

  testthat::expect_warning(check_geoData_names(gd2, rd2))

})

testthat::test_that('summaryWidget errors as expected', {

  correct_geoData = sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  incorrect_geoData = data.frame('a' = 3)

  correct_rtData = list()
  incorrect_rtData = 'a'

  testthat::expect_error(summaryWidget(incorrect_geoData, correct_rtData))

  testthat::expect_error(summaryWidget(correct_geoData, correct_rtData))

  testthat::expect_error(summaryWidget(correct_geoData, incorrect_rtData))

  rt <- list('a'=list('a' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'))

  testthat::expect_error(summaryWidget(correct_geoData, rt))

  rt <- list('a'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'))

  gd <- sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))

  testthat::expect_error(summaryWidget(gd, rt))

})





testthat::test_that('summaryWidget errors as expected', {
  
  correct_geoData = sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  incorrect_geoData = data.frame('a' = 3)
  
  correct_rtData = list()
  incorrect_rtData = 'a'
  
  testthat::expect_error(tsWidget(incorrect_geoData, correct_rtData))
  
  testthat::expect_error(tsWidget(correct_geoData, correct_rtData))
  
  testthat::expect_error(tsWidget(correct_geoData, incorrect_rtData))
  
  rt <- list('a'=list('a' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'))
  
  testthat::expect_error(tsWidget(correct_geoData, rt))
  
  rt <- list('a'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'))
  
  gd <- sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  
  testthat::expect_error(tsWidget(gd, rt))
  
})

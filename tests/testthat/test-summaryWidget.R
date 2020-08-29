testthat::test_that('define_height works for different dataset combinations', {
  
  height <- define_height(geoData = 'a', summaryData = 'b', rtData = list('a'=list('a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275)
  
  height <- define_height(geoData = NULL, summaryData = 'b', rtData = list('a'=list('a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275 - 500)
  
  height <- define_height(geoData = 'a', summaryData = NULL, rtData = list('a'=list('a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275 - 500)
  
  height <- define_height(geoData = 'a', summaryData = 'b', rtData = list('a'=list('a', 'b', NULL, 'd')))
  
  testthat::expect_equal(height, (1275 - 225))
  
  height <- define_height(geoData = 'a', summaryData = 'b', rtData = list('a'=list('a', NULL, NULL, 'd')))
  
  testthat::expect_equal(height, (1275 - 225 - 225))
  
})

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
  
  rd1 <- list('a'=list('a'=data.frame(country = c('z', 'y', 'x', 'w')), 'b' = NULL, 'c' = NULL))
  rd2 <- list('a'=list('a'=data.frame(country = c('z', 'y', 'x', 'w', 'v', 'u')), 'b' = NULL, 'c' = NULL))
  
  testthat::expect_warning(check_geoData_names(gd1, rd1))
  
  testthat::expect_warning(check_geoData_names(gd2, rd2))
  
})

testthat::test_that('check_geoData_columns works as expected', {
  
  expected_columns <- c('sovereignt', 'geometry')
  
  gd <- sf::st_sf(sovereignt=3, geometry = sf::st_sfc(sf::st_point(1:2)))

  testthat::expect_true(check_geoData_columns(gd, expected_columns))
  
  gd <- sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  
  testthat::expect_false(check_geoData_columns(gd, expected_columns))
  
})


testthat::test_that('check_obsCasesData_columns works as expected', {
  
  expected_columns <- c('region','date','confirm')
  
  correct_df <- data.frame(region = 'a', date = 'a', confirm = 'a')
  incorrect_df <- data.frame(a = 'a', date = 'a', confirm = 'a')
  
  rt <- list('a'=list('obsCasesData' = correct_df))
  
  testthat::expect_true(check_obsCasesData_columns(rt, expected_columns))
  
  rt <- list('a'=list('obsCasesData' = incorrect_df))
  
  testthat::expect_false(check_obsCasesData_columns(rt, expected_columns))
  
  rt <- list('a'=list('obsCasesData' = correct_df),
             'b'=list('obsCasesData' = correct_df))
  
  testthat::expect_true(check_obsCasesData_columns(rt, expected_columns))
  
  rt <- list('a'=list('obsCasesData' = incorrect_df),
             'b'=list('obsCasesData' = incorrect_df))
  
  testthat::expect_false(check_obsCasesData_columns(rt, expected_columns))
  
})

testthat::test_that('check_obsCasesData_columns works as expected', {
  
  expected_columns <- c('country','date','type','median','lower_90','upper_90','lower_50','upper_50')
  
  correct_df <- data.frame(country = 'a', date = 'a', type = 'a', 
                           median = 'a', lower_90 = 'a', upper_90 = 'a', 
                           lower_50 = 'a', upper_50 = 'a')
  
  incorrect_df <- data.frame(a = 'a', date = 'a', type = 'a', 
                           median = 'a', lower_90 = 'a', upper_90 = 'a', 
                           lower_50 = 'a', upper_50 = 'a')
  
  rt <- list('a'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_true(check_rtData_columns(rt, expected_columns))
  
  rt <- list('a'=list('rtData' = incorrect_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_false(check_rtData_columns(rt, expected_columns))
  
  rt <- list('a'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df),
             'b'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_true(check_rtData_columns(rt, expected_columns))
  
  rt <- list('a'=list('rtData' = incorrect_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df),
             'b'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_false(check_rtData_columns(rt, expected_columns))
  
  rt <- list('a'=list('rtData' = NULL, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df),
             'b'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_true(check_rtData_columns(rt, expected_columns))
  
  rt <- list('a'=list('rtData' = NULL, 'casesInfectionData' = incorrect_df, 'casesReportData' = correct_df),
             'b'=list('rtData' = correct_df, 'casesInfectionData' = correct_df, 'casesReportData' = correct_df))
  
  testthat::expect_false(check_rtData_columns(rt, expected_columns))
  
})

testthat::test_that('check_rtData_structure works as expected', {
  
  expected_names <- c("rtData", "casesInfectionData", "casesReportData", "obsCasesData")
  
  rt <- list('a'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'))
  
  testthat::expect_true(check_rtData_structure(rt, expected_names))
  
  rt <- list('a'=list('a' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'))
  
  testthat::expect_false(check_rtData_structure(rt, expected_names))
  
})
  
testthat::test_that('summaryWidget errors as expected', {
  
  correct_geoData = sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  incorrect_geoData = data.frame('a' = 3)
  
  correct_summaryData = data.frame('a' = 3)
  incorrect_summaryData = 'a'
  
  correct_rtData = list()
  incorrect_rtData = 'a'
  
  testthat::expect_error(summaryWidget(incorrect_geoData, correct_summaryData, correct_rtData))
  
  testthat::expect_error(summaryWidget(correct_geoData, incorrect_summaryData, correct_rtData))
  
  testthat::expect_error(summaryWidget(correct_geoData, correct_summaryData, incorrect_rtData))
    
  rt <- list('a'=list('a' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'))
  
  testthat::expect_error(summaryWidget(correct_geoData, correct_summaryData, rt))
  
  rt <- list('a'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a'))
  
  gd <- sf::st_sf(a=3, geometry = sf::st_sfc(sf::st_point(1:2)))
  
  testthat::expect_error(summaryWidget(gd, correct_summaryData, rt))
  
  
})




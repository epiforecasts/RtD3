
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

testthat::test_that('check_rtData_columns works as expected', {

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

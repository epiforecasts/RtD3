testthat::test_that('tsWidget errors as expected', {
  
  correct_rtData = list()
  incorrect_rtData = 'a'
  
  rt <- list('a'=list('a' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'),
             'b'=list('rtData' = 'a', 'casesInfectionData' = 'a', 'casesReportData' = 'a', "obsCasesData" = 'a', 'summaryData' = 'a'))
  
  testthat::expect_error(tsWidget(correct_geoData, rt))
  
})

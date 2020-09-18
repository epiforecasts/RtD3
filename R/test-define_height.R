testthat::test_that('define_height works for different dataset combinations', {
  
  height <- define_height(geoData = 'a', rtData = list('a'=list(summaryData = '1', 'a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275)
  
  height <- define_height(geoData = NULL, rtData = list('a'=list(summaryData = '1', 'a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275 - 500)
  
  height <- define_height(geoData = 'a', rtData = list('a'=list(summaryData = NULL, 'a', 'b', 'c', 'd')))
  
  testthat::expect_equal(height, 1275 - 500)
  
  height <- define_height(geoData = 'a', rtData = list('a'=list(summaryData = '1', 'a', 'b', NULL, 'd')))
  
  testthat::expect_equal(height, (1275 - 225))
  
  height <- define_height(geoData = 'a', rtData = list('a'=list(summaryData = '1', 'a', NULL, NULL, 'd')))
  
  testthat::expect_equal(height, (1275 - 225 - 225))
  
})
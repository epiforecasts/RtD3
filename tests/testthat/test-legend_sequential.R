testthat::test_that('legend_sequential returns appropriate values', {
  
  legend <-  legend_sequential('Anything', 'scaleLinear', 'white', 'green', 'lightgrey')
  
  testthat::expect_equal(legend[['variable_name']], 'Anything')
  testthat::expect_equal(legend[['legend_type']], 'sequential')
  testthat::expect_equal(legend[['legend_scale']], 'scaleLinear')
  testthat::expect_equal(legend[['legend_values']][['low']], 'white')
  testthat::expect_equal(legend[['legend_values']][['high']], 'green')
  testthat::expect_equal(legend[['legend_values']][['No Data']], 'lightgrey')
  
})
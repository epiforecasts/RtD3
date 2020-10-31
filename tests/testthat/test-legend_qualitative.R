testthat::test_that('legend_qualitative returns appropriate values', {
  
  legend <-  legend_qualitative('Anything', list('This' = 'That'))
  
  testthat::expect_equal(legend[['variable_name']], 'Anything')
  testthat::expect_equal(legend[['legend_type']], 'qualitative')
  testthat::expect_identical(legend[['legend_values']], list('This' = 'That'))
  
  testthat::expect_error(legend_qualitative('Anything', 'Anything'))
  
})
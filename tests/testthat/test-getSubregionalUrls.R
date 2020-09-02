context("getSubregionalUrls")

testthat::test_that("Test that getSubregionalUrls can construct the expected output", {
  out <- getSubregionalUrls(path = "hi/",
                            areas = c('there', 'their', "over there"))

  testthat::expect_equal(names(out), c("there", "their", "over there"))
  testthat::expect_equal(unname(out), c("hi/there/","hi/their/", "hi/over-there/"))
})

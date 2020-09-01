context("getSubregionalUrls")

test_that("Test that getSubregionalUrls can construct the expected output", {
  out <- getSubregionalUrls(path = "hi/",
                            areas = c('there', 'Their', "over there"))


  expect_equal(names(out), c("there", "Their", "over there"))
  expect_equal(unname(out), c("hi/there/","hi/their/", "hi/over-there/"))
})

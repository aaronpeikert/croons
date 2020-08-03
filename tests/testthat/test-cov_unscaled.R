test_that("unscaled cov is correct", {
  iris <- dplyr::select_if(iris, is.numeric)
  expect_equivalent(cov_unscaled(iris)/(nrow(iris) - 1), cov(iris))
})

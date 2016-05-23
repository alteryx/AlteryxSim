context("test-hyperparameter-sampling")

test_that("quantile_sample function correctly computes inverse", {
  expect_equal(
    quantile_sample("normal", seq(from = 0, to = 1, by = .25), list(mean = 0, sd = 1)), 
    c(-Inf, qnorm(.25), 0, qnorm(.75), Inf)
  )
})
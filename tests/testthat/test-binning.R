context("test_binning")

test_that("bin_to_data correctly converts df of ids/vals to vector of values", {
  binnedData <- data.frame(id = c(0,1,2), count = c(1,10,30))
  vecData <- bin_to_data(binnedData)
  bin0 <- vecData[1]
  bin1 <- vecData[2:11]
  bin2 <- vecData[12: 41]
  expect_gte(min(bin0), -.5)
  expect_lte(max(bin0), .5)
  expect_gte(min(bin1), .5)
  expect_lte(max(bin1), 1.5)
  expect_gte(min(bin2), 1.5)
  expect_lte(max(bin2), 2.5)
})

test_that("bin_to_data throws error on bins of various width", {
  binnedData <- data.frame(id = c(0,1,4), count = c(1,10,30))
  expect_that(bin_to_data(binnedData), throws_error("bins must have equal width"))
})
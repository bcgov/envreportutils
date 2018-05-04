context("test-utils.R")

test_that("report_percent works", {
  test_vec <- c(1.562, 9.765, 25.012, 90.326, 99.423, 99.675)
  expect_equal(report_percent(test_vec), 
               c("1.6", "9.8", "25", "90", "99", "99.7"))
  expect_equal(report_percent(test_vec, round_90_to_one = TRUE), 
               c("1.6", "9.8", "25", "90.3", "99.4", "99.7"))
  expect_equal(report_percent(test_vec, as_char = FALSE), 
               c(1.6, 9.8, 25.0, 90.0, 99.0, 99.7))
  expect_equal(report_percent(test_vec, round_90_to_one = TRUE, as_char = FALSE), 
               c(1.6, 9.8, 25.0, 90.3, 99.4, 99.7))
})

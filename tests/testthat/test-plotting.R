context("test-plotting")

suppressPackageStartupMessages(require("ggplot2", quietly = TRUE))

d <- data.frame(x = 1, y = 1)

p <- ggplot(d, aes(x = x, y = y)) + geom_point()

f1 <- tempfile(fileext = ".png")
test_that("png_retina works", {
  png_retina(f1)
  plot(p)
  dev.off()
  expect_true(file.exists(f1))
})

f2 <- tempfile(fileext = ".svg")
test_that("svg_px works", {
  svg_px(f2)
  plot(p)
  dev.off()
  expect_true(file.exists(f2))
})

f3 <- tempfile(fileext = ".png")
test_that("save_png_retina works", {
  save_png_retina(p, f3)
  expect_true(file.exists(f3))
  expect_equal(readBin(f3, "raw", 1000L), readBin(f1, "raw", 1000L))
})

f4 <- tempfile(fileext = ".svg")
test_that("save_png_retina works", {
  save_svg_px(p, f4)
  expect_true(file.exists(f4))
  expect_equal(readLines(f4), readLines(f2))
})


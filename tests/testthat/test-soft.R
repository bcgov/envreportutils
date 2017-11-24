context("test soft")

test_file <- tempfile()
cat("test\n", file = test_file, append = TRUE)
test_dir <- tempdir()
file.copy(test_file, test_dir, overwrite = TRUE)

test_that("zip_it works", {
  zipfile <- zip_it(test_dir)
  expect_true(file.exists(zipfile))
  expect_match(basename(zipfile), "^soft_send.+\\.zip$")
})

test_that("errors correctly", {
  expect_error(soft(file = "Asdfds190879"),
               "file does not exist")
  expect_error(soft(file = test_file, email = "fooatgmaildotcom"),
               "You have entered an invalid email address")
  expect_error(soft(file = test_file, email = c("foo@bar.com", "bar@foo.com")),
               "only one email address can be entered")
  expect_error(soft(test_file, internal = "blah"),
               "'internal' must be TRUE or FALSE")
  expect_error(soft(file = test_file, days = "seven"),
               "'days' must be a number")
})

test_that("soft works", {
  expect_match(soft(test_file, progress = FALSE), "^http://www\\.env\\.gov\\.bc\\.ca/perl/soft/dl\\.pl")
  expect_message(soft(test_dir, progress = FALSE), "Zipping")
})

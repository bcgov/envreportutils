context("test soft")

test_file <- tempfile()
cat("test\n", file = test_file, append = TRUE)
test_file2 <- tempfile()
cat("test\n", file = test_file2, append = TRUE)
test_dir <- tempdir()
file.copy(c(test_file, test_file2), test_dir, overwrite = TRUE)

test_that("zip_it works", {
  zipfile <- zip_it(test_dir)
  expect_true(file.exists(zipfile))
  expect_match(basename(zipfile), "^soft_send.+\\.zip$")
  zipfile2 <- zip_it(test_dir, zipname = "foo.zip")
  expect_true(file.exists(zipfile2))
  expect_match(basename(zipfile2), "^foo\\.zip$")
  zipfile3 <- zip_it(c(test_file, test_file2), zipname = "bar.zip")
  expect_true(file.exists(zipfile3))
  expect_match(basename(zipfile3), "^bar\\.zip$")
})

test_that("errors correctly", {
  expect_error(soft(file = "Asdfds190879"),
               "files do not exist")
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

context("test-bc_button_leaflet")
library(leaflet)

test_that("add_bc_home_button works", {
  m <- add_bc_home_button(leaflet())
  expect_is(m, "leaflet")
  expect_length(m$dependencies, 2L)
  expect_equal(vapply(m$dependencies, `[[`, "name", FUN.VALUE = character(1)), 
               c("leaflet-easybutton", "bc-home-button"))
})

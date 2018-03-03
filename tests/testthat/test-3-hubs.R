context("Test hubs endpoint")

Sys.setenv("onfleet_api_key" = "e9e288461c3c6fa211391c582b7ff8cf")

test_that("Doesn't error", {
  expect_silent(onfleet_get_hubs())
})
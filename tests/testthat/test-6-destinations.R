context("Test Destinations endpoint")

test_that("Add destination", {
  wrigley <<- onfleet_post_destinations("Wrigley", number = "1060",street = "W Addison St", city = "Chicago", state = "IL", postalCode = "60613", country = "USA")
  expect_true(wrigley$address$name == "Wrigley")
})

test_that("Read destination", {
  wrigley2 <<- onfleet_get_destination(wrigley$id)
  expect_true(wrigley2$address$name == "Wrigley")
})

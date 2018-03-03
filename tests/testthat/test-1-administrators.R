context("Test administrators endpoint")

Sys.setenv("onfleet_api_key" = "e9e288461c3c6fa211391c582b7ff8cf")

test_that("Contains super admin", {
  admins <<- onfleet_get_admins()
  expect_true(any(admins$type == "super"))
})

test_that("Can add admin", {
  onfleet_post_admins("Test Admin", "jimduquettesucked@gmail.com")
  admins <<- onfleet_get_admins()
  expect_true(any(admins$name == "Test Admin"))
})

test_that("Can update admin", {
  id <<- admins$id[admins$name == "Test Admin"]
  onfleet_put_admins(id, name = "Test Admin Update")
  admins <<- onfleet_get_admins()
  expect_true(any(admins$name == "Test Admin Update"))
})

test_that("Can delete admin", {
  onfleet_delete_admins(id)
  admins <<- onfleet_get_admins()
  expect_false(any(admins$name == "Test Admin Update"))
})

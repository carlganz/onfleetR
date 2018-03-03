context("Test teams endpoint")

Sys.setenv("onfleet_api_key" = "e9e288461c3c6fa211391c582b7ff8cf")

test_that("Can add team", {
  onfleet_post_teams("Test Team", NA, list(onfleet_get_admins()$id))
  teams <<- onfleet_get_teams()
  expect_true(any(teams$name == "Test Team"))
})

test_that("Can access specific team", {
  expect_true(length(onfleet_get_team(teams$id[teams$name == "Worker Test"]))>0)
})

test_that("Can update team", {
  id <<- teams$id[teams$name == "Test Team"]
  onfleet_put_teams(id, name = "Test Team Update")
  teams <<- onfleet_get_teams()
  expect_true(any(teams$name == "Test Team Update"))
})

test_that("Can delete admin", {
  onfleet_delete_teams(id)
  teams <<- onfleet_get_teams()
  expect_false(any(teams$name == "Test Team Update"))
})

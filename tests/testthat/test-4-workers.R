context("Test workers endpoint")

Sys.setenv("onfleet_api_key" = "e9e288461c3c6fa211391c582b7ff8cf")

teams <- onfleet_get_teams()

test_that("Can add worker", {
  onfleet_post_workers("Test Worker", "+13236004455", list(teams$id[teams$name == "Worker Test"]))
  workers <<- onfleet_get_workers()
  expect_true(any(workers$name == "Test Worker"))
})

test_that("Can update worker", {
  id <- as.character(workers$id[workers$name == "Test Worker"])
  onfleet_put_workers(id, name = "Test Worker Update")
  workers <<- onfleet_get_workers()
  expect_true(any(workers$name == "Test Worker Update"))
})




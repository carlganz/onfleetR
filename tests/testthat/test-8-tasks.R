context("Test tasks endpoint")

workers <- onfleet_get_workers()
id <- as.character(workers$id[workers$name == "Test Worker"])
wrigley <- onfleet_post_destinations(number = "1060",street = "W Addison St", city = "Chicago", state = "IL", postalCode = "60613", country = "USA")
# sox <- onfleet_post_destinations(number = "333", street = "W 35th St", city = "Chicago", state = "IL", postalCode = "60616", country = "USA")

recipient <- "sAWnWWGJZu5cymqfC2~8y2lY"

test_that("Create Task", {
  task <- onfleet_post_tasks(destination = wrigley$id, recipients = list(recipient))
  expect_true(length(task) > 0)
  # onfleet_post_tasks(destination = sox$id, recipients = list(recipient))
})

test_that("Get Tasks", {
  tasks <<- onfleet_get_tasks(Sys.Date() - 1)
  tasks <<- tasks[[1]][unlist(map(tasks[[1]], "state"))<3]
  expect_true(length(tasks) > 0)
  task <- onfleet_get_task(tasks[[c(1,1)]])
  expect_true(length(task) > 0)
})

test_that("Update tasks", {
  onfleet_put_task(tasks[[c(1,1)]], notes = "This has been updated.")
  task <- onfleet_get_task(tasks[[c(1,1)]])
  expect_true(task$notes == "This has been updated.")
})

test_that("Delete task", {
  deleted_id <- tasks[[c(1,1)]]
  expect_silent(onfleet_get_task(deleted_id))
  onfleet_delete_task(deleted_id)
  expect_error(onfleet_get_task(deleted_id))
})



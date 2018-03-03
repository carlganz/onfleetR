context("Test containers endpoint")

tasks <- onfleet_get_tasks(Sys.Date() - 1)
tasks <- tasks[[1]][unlist(map(tasks[[1]], "state"))<3]
workers <- onfleet_get_workers()

worker_id <- workers$id[1]

task_id <- tasks[[c(1,1)]]

test_that("Assign task to worker", {
  # errors
  expect_true(onfleet_put_insert_tasks("workers", worker_id, -1, task_id))
})

### These have to happen at end
# the task has to be activated in real life
# test_that("Complete task", {
#   complete_id <- task_id
#   onfleet_post_complete_task(complete_id, TRUE, "We did it!")
#   task <- onfleet_get_task(complete_id)
# })

test_that("Can delete worker", {
  onfleet_delete_workers(worker_id)
  workers <<- onfleet_get_workers()
  expect_false(any(workers$name == "Test Worker Update"))
})


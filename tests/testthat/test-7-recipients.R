context("Test recipients endpoint")


Sys.setenv("onfleet_api_key" = "e9e288461c3c6fa211391c582b7ff8cf")

name <- paste0(sample(LETTERS, 15, replace = TRUE), collapse = "")
phone <- paste0("+1", paste0(sample(0:9, 10, replace = TRUE), collapse = ""))

test_that("Can add recipient", {
  recipient <- onfleet_post_recipients(name, phone, skipSMSNotifications = TRUE, skipPhoneNumberValidation = TRUE)
  expect_true(recipient$name == name)
  expect_true(recipient$phone == phone)
})

test_that("Can find recipient by name", {
  recipient <- onfleet_find_recipients(name = name)
  expect_true(recipient$name == name)
})

test_that("Can find recipient by phone", {
  recipient <- onfleet_find_recipients(phone = phone)
  id <<- recipient$id
  expect_true(recipient$phone == phone)
})

test_that("Can get recipient", {
  recipient <- onfleet_get_recipient(id)
  expect_true(recipient$phone == phone)
  expect_true(recipient$name == name)
})

name <- paste0(sample(LETTERS, 15, replace = TRUE), collapse = "")

test_that("Can update recipient", {
  onfleet_put_recipients(id, name = name)
  recipient <- onfleet_get_recipient(id)
  expect_true(recipient$name == name)
})

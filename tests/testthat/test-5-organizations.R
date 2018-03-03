context("Test organizations endpoint")

test_that("Read all organizations", {
  organizations <- onfleet_get_organizations()
  expect_true(organizations$name == "CannaData")
})

test_that("Read single organization", {
  organization <- onfleet_get_organization(organizations$id)
  expect_true(organization$name == "CannaData")
})

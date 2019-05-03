context("Context for Checkers")

#check prob
test_that("check that probability function works", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(2))
  expect_length(check_prob(0.5), 1)
})

#check trials
test_that("check that trials function works", {
  expect_true(check_trials(1))
  expect_error(check_trials(-2))
  expect_length(check_trials(3), 1)
})

#check success
test_that("check that success function works", {
  expect_true(check_success(1, 3))
  expect_error(check_success(3, 1))
  expect_true(check_success(3, 7))
})





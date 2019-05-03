context("Context for binomial")

#bin_choose()
test_that("check that bin_choose() function works", {
  expect_equal(bin_choose(5, 2), 10)
  expect_is(bin_choose(8, 3), "numeric")
  expect_gte(bin_choose(10, 3), 3)
})


#bin_probability()
test_that("check that bin_probability() function works", {
  expect_equal(bin_probability(1, 3, 0.5), 0.375)
  expect_is(bin_probability(7, 10, 0.5), "numeric")
  expect_error(bin_probability(3, 2, 0.7), 'invalid success value')
})


#bin_distribution()
test_that("check that bin_distribution() function works", {
  expect_type(bin_distribution(trials = 3, prob = 0.4), "list")
  expect_is(bin_distribution(trials = 5, prob = 0.5), "data.frame")
  expect_error(bin_distribution(trials = 5, prob = -0.5), "p has to be a number betwen 0 and 1")
})


#bin_cumulative()
test_that("check that bin_cumulative() function works", {
  expect_error(bin_cumulative(trials = 5, prob = -0.5), "p has to be a number betwen 0 and 1")
  expect_type(bin_cumulative(trials = 5, prob = 0.5), "list")
  expect_is(bin_cumulative(trials = 3, prob = 0.4), "data.frame")
})

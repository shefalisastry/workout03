context("Context for summary measures")
#source("C:/Users/lib_laptop/Documents/binomial/binomial.R")

#getwd()
#aux_mean
test_that("check that mean function works", {
expect_equal(aux_mean(2, 0.5), 1)
expect_equal(aux_mean(3, 0.5), 1.5)
expect_is(aux_mean(10, 0.5), "numeric")
expect_lte(aux_mean(10, 0.5), 10)
})

#aux_variance
test_that("check that variance function works", {
expect_equal(aux_variance(8, 0.2), 1.28)
expect_equal(aux_variance(10, 0.7), 2.1)
expect_is(aux_variance(10, 0.3), "numeric")
})

#aux_mode
test_that("check that mode function works", {
  expect_equal(aux_mode(10, 0.2), 2)
  expect_length(aux_mode(10, 0.1), 1)
  expect_is(aux_mode(20, 0.1), "numeric")
})

#aux_skewness
test_that("check that skewness function works", {
  expect_equal(aux_skewness(5, 0.5), 0)
  expect_lte(aux_skewness(10, 0.1), 1)
  expect_is(aux_skewness(20, 0.1), "numeric")
})


#aux_kurtosis
test_that("check that kurtosis function works", {
  expect_lte(aux_kurtosis(5, 0.5), 0)
  expect_equal(aux_kurtosis(8, 0.1), 0.405)
  expect_is(aux_kurtosis(20, 0.1), "numeric")
})












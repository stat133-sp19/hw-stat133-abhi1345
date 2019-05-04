context("Testing Checkers")

test_that("Check the probability checker fuction.", {
  expect_equal(check_prob(0.5), TRUE)
  expect_error(check_prob(c(2, 3)))
  expect_error(check_prob(-1))
})

test_that("Check the trials checker function.", {

  expect_equal(check_trials(5), TRUE)
  expect_error(check_trials(-1))
  expect_error(check_trials(0.5))
})

test_that("Check the success checker function.", {

  expect_equal(check_success(5, 6), TRUE)
  expect_error(check_success(6, 5))
  expect_error(check_success(0, -1))
})


context("Testing Summary Measures")

test_that("Check the auxiliary mean function.", {

  expect_equal(aux_mean(5, .6), 3)
  expect_equal(aux_mean(3, .2), 0.6)
  expect_equal(aux_mean(10, .2), 2)
})

test_that("Check the auxiliary variance function.", {

  expect_equal(aux_variance(5, .6), 1.2)
  expect_equal(aux_variance(3, .2), 0.48)
  expect_equal(aux_variance(10, .2), 1.6)
})

test_that("Check the auxiliary mode function.", {

  expect_equal(aux_mode(5, .6), 3)
  expect_equal(aux_mode(3, .2), 0)
  expect_equal(aux_mode(10, .2), 2)
})

test_that("Check the auxiliary skewness function.", {

  expect_equal(aux_skewness(5, .6), -0.1825742)
  expect_equal(aux_skewness(3, .2), 0.8660254)
  expect_equal(aux_skewness(10, .5), 0)
})

test_that("Check the auxiliary kurtosis function.", {

  expect_equal(aux_kurtosis(5, .5), -0.4)
  expect_equal(aux_kurtosis(3, .2), 0.08333333)
  expect_equal(aux_kurtosis(10, .2), 0.025)
})


context("Testing Binomial Function")

test_that("Check the binomial choose function.", {

  expect_equal(bin_choose(5, 2), 10)
  expect_error(bin_choose(5, 6))
  expect_equal(bin_choose(7, 1), 7)
})

test_that("Check the binomial probability function.", {

  expect_equal(bin_probability(1, 2, 0.5), 0.5)
  expect_equal(bin_probability(6, 6, 0.5), 0.015625)
  expect_error(bin_probability(6, 5, 0.5))
})

test_that("Check the binomial distribution function.", {

  expect_equal(nrow(bin_distribution(5, .5)), 6)
  expect_length(bin_distribution(5, .5), 2)
  expect_error(bin_distribution(-1, .5))
})

test_that("Check the binomial cumulative function.", {

  expect_equal(nrow(bin_cumulative(7, .5)), 8)
  expect_length(bin_cumulative(5, .5), 3)
  expect_error(bin_cumulative(-1, .5))
})

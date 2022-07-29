source("helpers.R")

test_that("zero inflated poisson distribution has correct density", {

  skip_if_not(check_tf_version())
  

  compare_distribution(zero_inflated_poisson,
                       extraDistr::dzip,
                       parameters = list(theta = 0.2, lambda = 2, pi = 0.2),
                       x = sample_zero_inflated_pois(100, 2, 0.2))

})

test_that("zero inflated negative binomial distribution has correct density", {

  skip_if_not(check_tf_version())

  compare_distribution(zero_inflated_negative_binomial,
                       extraDistr::dzinb,
                       parameters = list(theta = 2, size = 10, prob = 0.1, pi = 0.2),
                       x = extraDistr::rzinb(100, 10, 0.1, 0.2))

})

source("helpers.R")

test_that("zero inflated poisson distribution has correct density", {
  skip_if_not(check_tf_version())

  compare_distribution(
    greta_fun = zero_inflated_poisson,
    r_fun = extraDistr::dzip,
    parameters = list(lambda = 2, pi = 0.2),
    x = sample_zero_inflated_pois(
      n = 100,
      lambda = 2,
      pi = 0.2
    )
  )
})

test_that("zero inflated poisson distribution works with calculate", {
  skip_if_not(check_tf_version())
  
  param_lambda <- 2
  param_pi <- 0.1
  example_zip <- zero_inflated_poisson(
    lambda = param_lambda, 
    pi = param_pi
  )
  
  expect_no_error(calculate(example_zip, nsim = 10))
  expect_no_error(m <- model(example_zip))
  expect_no_error(mcmc(m, n_samples = 10, warmup = 10))
})

test_that("zero inflated negative binomial distribution has correct density", {
  skip_if_not(check_tf_version())

  compare_distribution(
    greta_fun = zero_inflated_negative_binomial,
    r_fun = extraDistr::dzinb,
    parameters = list(size = 10, prob = 0.1, pi = 0.2),
    x = extraDistr::rzinb(
      n = 100, size = 10, prob = 0.1, pi = 0.2
    )
  )
})


test_that("zero inflated negative binomialdistribution works with calculate", {
  skip_if_not(check_tf_version())
  
  param_size <- 2
  param_prob <- 0.2
  param_pi <- 0.1
  
  example_zinb <- zero_inflated_negative_binomial(
    size = param_size,
    prob = param_prob,
    pi = param_pi
  )
  
  expect_no_error(calculate(example_zinb, nsim = 10))
  expect_no_error(m <- model(example_zinb))
  expect_no_error(mcmc(m, n_samples = 10, warmup = 10))
})

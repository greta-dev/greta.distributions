source("helpers.R")

test_that("multivariate_probit distribution has correct density", {
  skip_if_not(check_tf_version())
  
  # compare_distribution(
  #   greta_fun = multivariate_probit,
    # r_fun = extraDistr::m,
    # parameters = list(lambda = 2, pi = 0.2),
    # x = sample_zero_inflated_pois(
    #   n = 100,
    #   lambda = 2,
    #   pi = 0.2
    # )
  # )
})

test_that("multivariate_probit distribution works",{
  skip_if_not(check_tf_version())
  mp <- multivariate_probit(mean = matrix(rnorm(2), nrow = 2), C = diag(2))
  expect_no_error(
    calculate(mp, nsim = 1)
  )
})
source("helpers.R")

test_that("zero inflated poisson distribution has correct density", {

  skip_if_not(check_tf_version())
  
  compare_distribution(greta_fun = zero_inflated_poisson,
                       r_fun = extraDistr::dzip,
                       parameters = list(2, 0.2),
                       x = sample_zero_inflated_pois(
                         n = 100, 
                         lambda = 2, 
                         prob = 0.2)
                       )

})

test_that("zero inflated negative binomial distribution has correct density", {
  skip_if_not(check_tf_version())

  compare_distribution(
    greta_fun = zero_inflated_negative_binomial,
    r_fun = extraDistr::dzinb,
    parameters = list(10, 0.1, 0.2),
    x = extraDistr::rzinb(
      n = 100, size = 10, prob = 0.1, pi = 0.2
    )
  )
})

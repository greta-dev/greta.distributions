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

test_that("zero inflated negative binomial distribution has correct density", {
  skip_if_not(check_tf_version())

  compare_distribution(
    greta_fun = zero_inflated_negative_binomial,
    r_fun = extraDistr::dzinb,
    parameters = list(size = 10, prob = 0.1, pi = 0.2),
    x = extraDistr::rzinb(
      n = 100,
      size = 10,
      prob = 0.1,
      pi = 0.2
    )
  )
})

# test_that("samplers are unbiased for zip", {
#   skip_if_not(check_tf_version())
#
#   x <- zero_inflated_poisson(0.1, 0.2)
#   iid <- function(n) {
#     extraDistr::rzip(n = n, lamb = 0.1, pi = 0.2)
#   }
#
#   zip_checked <- check_samples(
#     x = x,
#     iid_function = iid,
#     one_by_one = TRUE
#   )
#
#   # do the plotting
#   qqplot_checked_samples(zip_checked)
#
#   # do a formal hypothesis test
#   stat <- ks_test_mcmc_vs_iid(lkj_checked)
#
#   expect_gte(stat$p.value, 0.01)
# })

test_that(
  "discrete_lognormal fails when given the wrong argument dimensions", {
    # check breaks is a vector with at least length 2
    expect_snapshot(
      error = TRUE,
      discrete_lognormal(p = 1, psi = 1, breaks = 1)
    )
    
    # check dim to be a positive scalar integer
    expect_snapshot(
      error = TRUE,
      discrete_lognormal(p = 1, psi = 1, breaks = c(1, 2), dim = 0.1)
    )
    
  })
# 
# test_that("discrete lognormal distribution has correct density", {
#   skip_if_not(check_tf_version())
#   
#   compare_distribution(
#     greta_fun = discrete_lognormal,
#     r_fun = extraDistr::ddnorm,
#     parameters = list(lambda = 2, pi = 0.2),
#     x = extraDistr::rdnorm(
#       n = 100,
#       mean = 0,
#       sd = 1
#     )
#   )
# })
# 
# test_that("discrete normal distribution has correct density", {
#   skip_if_not(check_tf_version())
#   
#   compare_distribution(
#     greta_fun = discrete_normal,
#     r_fun = extraDistr::ddnorm,
#     parameters = list(),
#     x = extraDistr::rzinb(
#       n = 100, size = 10, prob = 0.1, pi = 0.2
#     )
#   )
# })

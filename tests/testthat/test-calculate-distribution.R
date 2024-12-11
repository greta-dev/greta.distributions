test_that("calculate works with conditional_bernouilli", {
  cb <- conditional_bernoulli(
    p = matrix(c(0.1,0.9), ncol = 2),
    psi = c(0.9),
    dim = 1
  )
  expect_success(
    calculate(cb, nsim = 10)
  )
})


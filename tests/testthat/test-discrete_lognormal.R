test_that(
  "discrete_lognormal fails when given the wrong argument dimensions", {
    # check breaks is a vector with at least length 2
    expect_snapshot_error(
      discrete_lognormal(p = 1, psi = 1, breaks = 1)
    )
    
    # check dim to be a positive scalar integer
    expect_snapshot_error(
      discrete_lognormal(p = 1, psi = 1, breaks = c(1, 2), dim = 0.1)
    )
    
  })

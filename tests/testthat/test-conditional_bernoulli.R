# test_that("conditional_bernoulli distribution has correct density", {
#   skip_if_not(check_tf_version())
#   
#   
#   compare_distribution(greta.distributions::conditional_bernoulli,
#                        stats::dnorm,
#                        parameters = list(mean = -2, sd = 3),
#                        x = rnorm(100, -2, 3)
#   )
# })

test_that(
  "conditional_bernoulli fails when given the wrong argument dimensions", {
    # check p is a 2d array with at least 2 columns
    expect_snapshot_error(
      conditional_bernoulli(p = 1, psi = 1)
    )
    # check p and psi have the same number of rows
    expect_snapshot_error(
      conditional_bernoulli(
        p = matrix(c(1,2), ncol = 2), 
        psi = c(1,2)
        )
    )
    expect_snapshot_error(
      conditional_bernoulli(
        p = matrix(c(0.1,0.9), ncol = 2), 
        psi = matrix(c(1,2), nrow = 2)
        )
    )
    expect_snapshot_error(
      conditional_bernoulli(
        p = matrix(c(0.1,0.9), ncol = 2), 
        psi = c(0.9,0.1)
        )
    )
    expect_snapshot_error(
      conditional_bernoulli(
        p = matrix(c(0.1,0.9), ncol = 2), 
        psi = c(0.9),
        dim = 0.1
        )
    )
    expect_snapshot_error(
      conditional_bernoulli(
        p = matrix(c(0.1,0.9), ncol = 2), 
        psi = c(0.9),
        dim = 0.1
        )
    )
  
})
#' 
#' #' @param p matrix (of dimension `dim` x K) of (conditional) probabilities
#' #'   of success
#' #' @param psi scalar or column vector (of length `dim`) of probabilities
#' #'   for the latent bernoulli variable
#' 
# check dimensions of psi
# if (ncol(psi) != 1 | length(dim(psi)) != 2) {
#   msg <- cli::format_error(
#     c(
#       "{.var psi} must be a 2D greta array with one column",
#       "but {.var psi} has dimensions {paste(dim(psi), collapse = 'x')}"
#     )
#   )


# compare possible dimensions
# dim_p <- nrow(p)
# dim_psi <- nrow(psi)
# 
# if (dim_p != dim_psi) {
#   msg <- cli::format_error(
#     c(
#       "{.var p} and {.var psi} must have the same number of rows",
#       "But we see {.var p} and {.var psi} have:",
#       "{.var p}: {dim_p} {?row/rows}",
#       "{.var psi}: {dim_psi} {?row/rows}",
#       "Perhaps you need to coerce {.var p} or {.var psi} to an \\
#             appropriate matrix?"
#     )

# # check dim is a positive scalar integer
# dim_old <- dim
# dim <- as.integer(dim)
# if (length(dim) > 1 || dim <= 0 || !is.finite(dim)) {
#   msg <- cli::format_error(
#     c(
#       "{.var dim} must be a scalar positive integer, but was:",
#       "{capture.output(dput(dim_old))}"
#     )
#   )
# 

write_distribution_test <- function(dist_name){
  template <- '
test_that("{dist_name} distribution has correct density", {
  skip_if_not(check_tf_version())
  
  compare_distribution(
    greta_fun = {dist_name},
    r_fun = TODO-YOUR-RANDOM-SAMPLING-FUNCTION,
    # e.g., rnorm, extraDist::...
    # see for example:
    parameters = list(lambda = 2, pi = 0.2),
    x = extraDist::rzip(
      n = 100,
      lambda = 2,
      pi = 0.2
    )
  )
})
'
  templated_test <- glue::glue(template)
  # test test file exists...
  
  
}

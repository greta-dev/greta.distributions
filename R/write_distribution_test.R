write_distribution_test <- function(dist_name){
  template <- '
test_that("{dist_name} distribution has correct density", {
  skip_if_not(check_tf_version())
  
  compare_distribution(
    greta_fun = {dist_name},
    r_fun = TODO-YOUR-RANDOM-SAMPLING-FUNCTION,
    # e.g., rnorm, 
    #
    parameters = list(lambda = 2, pi = 0.2),
    x = sample_zero_inflated_pois(
      n = 100,
      lambda = 2,
      pi = 0.2
    )
  )
})
'
}

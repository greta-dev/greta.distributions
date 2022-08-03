# tensorflow functions

# CDF of the provided distribution, handling 0s and Infs
tf_safe_cdf <- function(x, distribution, lower_bound, upper_bound) {
  
  # prepare to handle values outside the supported range
  too_low <- tf$less(x, lower_bound)
  too_high <- tf$greater(x, upper_bound)
  supported <- !too_low & !too_high
  ones <- tf$ones_like(x)
  zeros <- tf$zeros_like(x)
  
  # run cdf on supported values, and fill others in with the appropriate value
  x_clean <- tf$where(supported, x, ones)
  cdf_clean <- distribution$cdf(x_clean)
  mask <- tf$where(supported, ones, zeros)
  add <- tf$where(too_high, ones, zeros)
  cdf_clean * mask + add
  
}

# cast to integer
tf_as_integer <- function(x) {
  tf$cast(x, tf$int32)
}
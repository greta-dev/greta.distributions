#' @name zero_inflated_poisson
#' @title Zero Inflated Poisson distribution
#'
#' @description A zero inflated poisson distribution.
#'
#' @param theta proportion of zeros
#' @param lambda rate parameter
#' @param dim a scalar giving the number of rows in the resulting greta array
#' @importFrom R6 R6Class
#' @export
zero_inflated_poisson <- function (theta, lambda, dim = NULL) {
  distrib('zero_inflated_poisson', theta, lambda, dim)
}

#' @importFrom R6 R6Class
zero_inflated_poisson_distribution <- R6::R6Class(
  classname = "zero_inflated_poisson_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(theta, lambda, dim) {
      theta <- as.greta_array(theta)
      lambda <- as.greta_array(lambda)
      # add the nodes as children and parameters
      dim <- check_dims(theta, lambda, target_dim = dim)
      super$initialize("zero_inflated_poisson", dim, discrete = TRUE)
      self$add_parameter(theta, "theta")
      self$add_parameter(lambda, "lambda")
    },
    
    tf_distrib = function(parameters, dag) {
      theta <- parameters$theta
      lambda <- parameters$lambda
      log_prob <- function(x) {
        tf$math$log(
          theta * 
            tf$nn$relu(fl(1) - x) + 
            (fl(1) - theta) * 
            tf$pow(lambda, x) * 
            tf$exp(-lambda) / tf$exp(tf$math$lgamma(x + fl(1)))
        )
      }
      
      sample <- function(seed) {
        binom <- tfp$distributions$Binomial(total_count = 1, probs = theta)
        pois <- tfp$distributions$Poisson(rate = lambda)
        
        zi <- binom$sample(seed = seed)
        lbd <- pois$sample(seed = seed)
        
        (fl(1) - zi) * lbd
        
      }
      
      list(
        log_prob = log_prob,
        sample = sample,
        cdf = NULL,
        log_cdf = NULL
      )
    },
    
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
  )
)
# NOTE - not sure what to do here with the module stuff?
# distribution_classes_module <-
#   module(
#     zero_inflated_poisson_distribution,
#     zero_inflated_negative_binomial_distribution
#   )
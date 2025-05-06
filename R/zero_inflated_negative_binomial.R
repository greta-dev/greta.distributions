#' @name zero_inflated_negative_binomial
#' @title Zero Inflated Negative Binomial
#' @description A Zero Inflated Negative Binomial distribution
#' @param size positive integer parameter
#' @param prob probability parameter (`0 < prob < 1`),
#' @param pi proportion of zeros
#' @param dim a scalar giving the number of rows in the resulting greta array
#' @examples
#' \dontrun{
#' zinb <- zero_inflated_negative_binomial(size = 2, prob= 0.2, pi = 0.10)
#' calculate(zinb, nsim = 10)
#' m <- model(zinb)
#' mcmc(m)
#' }
#' @export
zero_inflated_negative_binomial <- function(size, prob, pi, dim = NULL) {
  distrib("zero_inflated_negative_binomial", size, prob, pi, dim)
}

zero_inflated_negative_binomial_distribution <- R6::R6Class(
  "zero_inflated_negative_binomial_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(size, prob, dim) {
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)
      
      # add the nodes as parents and parameters
      dim <- check_dims(size, prob, target_dim = dim)
      super$initialize("negative_binomial", dim, discrete = TRUE)
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
    },
    
    # nolint start
    tf_distrib = function(parameters, dag) {
      tfp$distributions$NegativeBinomial(
        total_count = parameters$size,
        probs = fl(1) - parameters$prob
      )
    }
    # nolint end
  )
)
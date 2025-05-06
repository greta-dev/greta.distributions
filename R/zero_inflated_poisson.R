#' @name zero_inflated_poisson
#' @title Zero Inflated Poisson distribution
#'
#' @description A zero inflated poisson distribution.
#'
#' @param lambda rate parameter
#' @param pi proportion of zeros
#' @param dim a scalar giving the number of rows in the resulting greta array
#' @examples
#' \dontrun{
#' zip <- zero_inflated_poisson(lambda = 2, pi = 0.2)
#' calculate(zip, nsim = 10)
#' mcmc(m)
#' }
#' @export
zero_inflated_poisson <- function(lambda, pi, dim = NULL) {
  distrib('zero_inflated_poisson', lambda, pi, dim)
}

#' @importFrom R6 R6Class
zero_inflated_poisson_distribution <- R6::R6Class(
  classname = "zero_inflated_poisson_distribution",
  inherit = distribution_node,
  public = list(
    initialize = function(lambda, pi, dim) {
      lambda <- as.greta_array(lambda)
      pi <- as.greta_array(pi)
      # add the nodes as children and parameters
      dim <- check_dims(lambda, pi, target_dim = dim)
      super$initialize("zero_inflated_poisson", dim, discrete = TRUE)
      self$add_parameter(lambda, "lambda")
      self$add_parameter(pi, "pi")
    },

    tf_distrib = function(parameters, dag) {
      lambda <- parameters$lambda
      pi_var <- parameters$pi
      log_prob <- function(x) {
        tf$math$log(
          (pi_var *
            (fl(1) - tf$math$sign(tf$math$abs(x))) +
            tf$math$exp(
              tf$math$log1p(-pi_var) -
                lambda +
                x * tf$math$log(lambda) -
                tf$math$lgamma(x + fl(1))
            ))
        )
      }

      sample <- function(seed) {
        binom <- tfp$distributions$Binomial(total_count = 1, probs = pi)
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

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
    initialize = function(size, prob, pi, dim) {
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)
      pi <- as.greta_array(pi)
      # add the nodes as children and parameters
      dim <- check_dims(size, prob, pi, target_dim = dim)
      super$initialize("zero_inflated_negative_binomial", dim, discrete = TRUE)
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
      self$add_parameter(pi, "pi")
    },
    tf_distrib = function(parameters, dag) {
      size <- parameters$size
      p <- parameters$prob # probability of success
      pi_var <- parameters$pi
      q <- fl(1) - parameters$prob
      log_prob <- function(x) {
        tf$math$log(
          (pi_var *
            (fl(1) - tf$math$sign(tf$math$abs(x))) +
            tf$math$exp(
              tf$math$log1p(-pi_var) +
                tf_lchoose(x + size - fl(1), x) +
                size * tf$math$log(p) +
                x * tf$math$log1p(-p)
            ))
        )
      }

      sample <- function(seed) {
        binom <- tfp$distributions$Binomial(total_count = 1, probs = pi_var)
        negbin <-
          tfp$distributions$NegativeBinomial(total_count = size, probs = q) # change of proba / parametrisation in 'stats'

        zi <- binom$sample(seed = seed)
        lbd <- negbin$sample(seed = seed)

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

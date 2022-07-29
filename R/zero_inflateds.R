zero_inflated_poisson_distribution <- R6Class(
  "zero_inflated_poisson_distribution",
  inherit = greta::.internals$nodes$node_classes$distribution_node,
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
        
        tf$math$log(theta * tf$nn$relu(fl(1) - x) + (fl(1) - theta) * tf$pow(lambda, x) * tf$exp(-lambda) / tf$exp(tf$math$lgamma(x + fl(1))))
      }
      
      sample <- function(seed) {
        
        binom <- tfp$distributions$Binomial(total_count = 1, probs = theta)
        pois <- tfp$distributions$Poisson(rate = lambda)
        
        zi <- binom$sample(seed = seed)
        lbd <- pois$sample(seed = seed)
        
        (fl(1) - zi) * lbd
        
      }
      
      list(log_prob = log_prob, sample = sample, cdf = NULL, log_cdf = NULL)
    },
    
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
  )
)


zero_inflated_negative_binomial_distribution <- R6Class(
  "zero_inflated_negative_binomial_distribution",
  inherit = greta::.internals$nodes$node_classes$distribution_node,
  public = list(
    initialize = function(theta, size, prob, dim) {
      theta <- as.greta_array(theta)
      size <- as.greta_array(size)
      prob <- as.greta_array(prob)
      # add the nodes as children and parameters
      dim <- check_dims(theta, size, prob, target_dim = dim)
      super$initialize("zero_inflated_negative_binomial", dim, discrete = TRUE)
      self$add_parameter(theta, "theta")
      self$add_parameter(size, "size")
      self$add_parameter(prob, "prob")
    },
    
    tf_distrib = function(parameters, dag) {
      theta <- parameters$theta
      size <- parameters$size
      p <- parameters$prob # probability of success
      q <- fl(1) - parameters$prob 
      log_prob <- function(x) {
        
        tf$math$log(theta * tf$nn$relu(fl(1) - x) + (fl(1) - theta) * tf$pow(p, size) * tf$pow(q, x) * tf$exp(tf$math$lgamma(x + size)) / tf$exp(tf$math$lgamma(size)) / tf$exp(tf$math$lgamma(x + fl(1))))
        
      }
      
      sample <- function(seed) {
        
        binom <- tfp$distributions$Binomial(total_count = 1, probs = theta)
        negbin <- tfp$distributions$NegativeBinomial(total_count = size, probs = q) # change of proba / parametrisation in 'stats'
        
        zi <- binom$sample(seed = seed)
        lbd <- negbin$sample(seed = seed)
        
        (fl(1) - zi) * lbd
        
      }
      
      list(log_prob = log_prob, sample = sample, cdf = NULL, log_cdf = NULL)
    },
    
    tf_cdf_function = NULL,
    tf_log_cdf_function = NULL
  )
)

#' @rdname distributions
#' @export
zero_inflated_poisson <- function (theta, lambda, dim = NULL) {
  distrib('zero_inflated_poisson', theta, lambda, dim)
}

#' @rdname distributions
#' @export
zero_inflated_negative_binomial <- function (theta, size, prob, dim = NULL) {
  distrib('zero_inflated_negative_binomial', theta, size, prob, dim)
}

distribution_classes_module <- module(zero_inflated_poisson_distribution,
                                      zero_inflated_negative_binomial_distribution)
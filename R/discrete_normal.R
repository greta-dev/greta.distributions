#' @name discrete_normal
#' @title Discrete normal distribution
#'
#' @description a discretised normal distribution (i.e. sampled by applying 
#'   the round operation to samples from a normal). Due to the numerical 
#'   instability of integrating across the distribution, a vector of breaks 
#'   must be defined and the observations will be treated as censored 
#'   within those breaks
#'
#' @param mean unconstrained parameters giving the mean of the distribution
#' @param sd unconstrained parameters giving the standard deviation of the 
#'   distribution
#' @param breaks a vector of breaks; observations will be treated as censored 
#'   within those breaks
#' @param dim a scalar giving the number of rows in the resulting greta array
#'
#' @importFrom R6 R6Class
#' @export

discrete_normal <- function(mean, sd, breaks, dim = NULL) {
  distrib("discrete_normal", mean, sd, breaks, dim)
}

# define the discrete normal distribution
discrete_normal_distribution <- R6Class(
  classname = "discrete_normal_distribution",
  inherit = distribution_node,
  public = list(
    
    breaks = NA,
    lower_bounds = NA,
    upper_bounds = NA,
    lower_bound = NA,
    upper_bound = NA,
    
    initialize = function(mean, sd, breaks, dim) {
      
      mean <- as.greta_array(mean)
      sd   <- as.greta_array(sd)
      
      # check length of breaks
      check_break_length(breaks)
      
      # add breaks, vector of lower and upper bounds, and the lower and upper
      # bound of supported values
      self$breaks <- breaks
      self$lower_bounds <- breaks[-length(breaks)]
      self$upper_bounds <- breaks[-1]
      self$lower_bound <- min(breaks)
      self$upper_bound <- max(breaks)
      
      # add the nodes as parents and parameters
      dim <- check_dims(mean, sd, target_dim = dim)
      super$initialize("discrete_normal", dim, discrete = TRUE)
      self$add_parameter(mean, "mean")
      self$add_parameter(sd, "sd")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      mean <- parameters$mean
      sd <- parameters$sd
      
      tf_breaks <- fl(self$breaks)
      tf_lower_bounds <- fl(self$lower_bounds)
      tf_upper_bounds <- fl(self$upper_bounds)
      tf_lower_bound <- fl(self$lower_bound)
      tf_upper_bound <- fl(self$upper_bound)
      
      log_prob <- function(x) {
        
        # build distribution object
        d <- tfp$distributions$Normal(
          loc = mean,
          scale = sd
        )
        
        # for those lumped into groups,
        # compute the bounds of the observed groups
        # and get tensors for the bounds in the format expected by TFP
        tf_idx <- tfp$stats$find_bins(x, tf_breaks)
        tf_idx_int <- tf_as_integer(tf_idx)
        tf_lower_vec <- tf$gather(tf_lower_bounds, tf_idx_int)
        tf_upper_vec <- tf$gather(tf_upper_bounds, tf_idx_int)
        
        # compute the density over the observed groups
        # note-to-self: this looks like https://mc-stan.org/docs/2_29/stan-users-guide/bayesian-measurement-error-model.html#rounding
        low <- tf_safe_cdf(tf_lower_vec, d, tf_lower_bound, tf_upper_bound)
        up <- tf_safe_cdf(tf_upper_vec, d, tf_lower_bound, tf_upper_bound)
        log_density <- log(up - low)
        
      }
      
      sample <- function(seed) {
        
        d <- tfp$distributions$Normal(
          loc = mean,
          scale = sd
        )
        continuous <- d$sample(seed = seed)
        # tf$round(continuous)
        
        # gather samples at the breaks to convert them to rounded values
        # while avoiding the use of round or floor, which assumes that 
        # the breaks are all the integers
        # first, create the lower and upper bounds that will bin a continuous
        # variable into breaks
        # using midpoint between breaks for now (from diff)
        # generate edges using midpoint, -Inf, and Inf
        breaks_diff <- diff(self$breaks)
        edges <- c(-Inf, self$lower_bounds + breaks_diff, Inf)
        # ditto from what we did to breaks above
        tf_edges <- fl(edges)
        tf_edges_idx <- tfp$stats$find_bins(continuous, tf_edges)
        tf_edges_idx_int <- tf_as_integer(tf_edges_idx)
        tf$gather(tf_breaks, tf_edges_idx_int)

      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)
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
    
    initialize = function(mean, sd, breaks, dim) {
      
      mean <- as.greta_array(mean)
      sd   <- as.greta_array(sd)
      
      # check length of breaks
      if (length(breaks) <= 1) {
        msg <- cli::format_error(
          c(
            "{.var breaks} must be a vector with at least two break points",
            "but {.var breaks} has length {length(breaks)}"
          )
        ) 
        stop(
          msg,
          call. = FALSE
        )
      }
      
      # handle gradient issue between sdlog and 0s
      breaks <- pmax(breaks, .Machine$double.eps)
      self$breaks <- breaks
      self$lower_bounds <- breaks[-length(breaks)]
      self$upper_bounds <- breaks[-1]
      
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
      
      log_prob <- function(x) {
        
        # build distribution object
        d <- tfp$distributions$Normal(
          loc = mean,
          scale = sd
        )
        
        # for those lumped into groups,
        # compute the bounds of the observed groups
        # and get tensors for the bounds in the format expected by TFP
        x_safe <- tf$math$maximum(x, fl(.Machine$double.eps))
        tf_idx <- tfp$stats$find_bins(x_safe, tf_breaks)
        tf_idx_int <- greta:::tf_as_integer(tf_idx)
        tf_lower_vec <- tf$gather(tf_lower_bounds, tf_idx_int)
        tf_upper_vec <- tf$gather(tf_upper_bounds, tf_idx_int)
        
        # compute the density over the observed groups
        low <- tf_safe_cdf(tf_lower_vec, d)
        up <- tf_safe_cdf(tf_upper_vec, d)
        log_density <- log(up - low)
        
      }
      
      sample <- function(seed) {
        
        d <- tfp$distributions$Normal(
          loc = mean,
          scale = sd
        )
        continuous <- d$sample(seed = seed)
        # tf$floor(continuous)
        tf$round(continuous)
        
      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)
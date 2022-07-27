#' @name discrete_lognormal
#' @title Discrete lognormal distribution
#'
#' @description a discretised lognormal distribution (i.e. sampled by applying 
#'   the floor operation to samples from a lognormal). Due to the numerical 
#'   instability of integrating across the distribution, a vector of breaks 
#'   must be defined and the observations will be treated as censored 
#'   within those breaks
#'
#' @param meanlog unconstrained parameters giving the mean of the distribution 
#'   on the log scale
#' @param sdlog unconstrained parameters giving the standard deviation of the 
#'   distribution on the log scale
#' @param breaks a vector of breaks; observations will be treated as censored 
#'   within those breaks
#' @param dim a scalar giving the number of rows in the resulting greta array
#'
#' @importFrom R6 R6Class
#' @export
 
discrete_lognormal <- function(meanlog, sdlog, breaks, dim = NULL) {
  distrib("discrete_lognormal", meanlog, sdlog, breaks, dim)
}

# define the discrete lognormal distribution
discrete_lognormal_distribution <- R6Class(
  classname = "discrete_lognormal_distribution",
  inherit = distribution_node,
  public = list(
    
    breaks = NA,
    lower_bounds = NA,
    upper_bounds = NA,
    
    initialize = function(meanlog, sdlog, breaks, dim) {
      
      meanlog <- as.greta_array(meanlog)
      sdlog   <- as.greta_array(sdlog)
      
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
      
      # check dim is a positive scalar integer
      dim_old <- dim
      dim <- as.integer(dim)
      if (length(dim) > 1 || dim <= 0 || !is.finite(dim)) {
        msg <- cli::format_error(
          c(
            "{.var dim} must be a scalar positive integer, but was:",
            "{dim_old}"
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
      dim <- check_dims(meanlog, sdlog, target_dim = dim)
      super$initialize("discrete_lognormal", dim, discrete = TRUE)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
      
    },
    
    tf_distrib = function(parameters, dag) {
      
      meanlog <- parameters$meanlog
      sdlog <- parameters$sdlog
      
      tf_breaks <- fl(self$breaks)
      tf_lower_bounds <- fl(self$lower_bounds)
      tf_upper_bounds <- fl(self$upper_bounds)
      
      log_prob <- function(x) {
        
        # build distribution object
        d <- tfp$distributions$LogNormal(
          loc = meanlog,
          scale = sdlog
        )
        
        # for those lumped into groups,
        # compute the bounds of the observed groups
        # and get tensors for the bounds in the format expected by TFP
        x_safe <- tf$math$maximum(x, fl(.Machine$double.eps))
        tf_idx <- tfp$stats$find_bins(x_safe, tf_breaks)
        tf_idx_int <- tf_as_integer(tf_idx)
        tf_lower_vec <- tf$gather(tf_lower_bounds, tf_idx_int)
        tf_upper_vec <- tf$gather(tf_upper_bounds, tf_idx_int)
        
        # compute the density over the observed groups
        low <- tf_safe_cdf(tf_lower_vec, d)
        up <- tf_safe_cdf(tf_upper_vec, d)
        log_density <- log(up - low)
        
      }
      
      sample <- function(seed) {
        
        d <- tfp$distributions$LogNormal(
          loc = meanlog,
          scale = sdlog
        )
        continuous <- d$sample(seed = seed)
        tf$floor(continuous)
        
      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
    
  )
)
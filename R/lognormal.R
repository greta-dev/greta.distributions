lognormal <- R6Class(
  classname = "lognormal",
  inherit = distribution_node,
  public = list(
    meanlog = NA,
    sdlog = NA,
    initialize = function(
        meanlog, sdlog,
        dim) {
      meanlog <- as.greta_array(meanlog)
      sdlog <- as.greta_array(sdlog)
      self$meanlog <- meanlog
      self$sdlog <- sdlog

      # add the nodes as parents and parameters
      dim <- check_dims(meanlog, sdlog,
        target_dim = dim
      )
      super$initialize("lognormal", dim, discrete = SET_OPTION)
      self$add_parameter(meanlog, "meanlog")
      self$add_parameter(sdlog, "sdlog")
    },
    tf_distrib = function(parameters, dag) {
      meanlog <- parameters$meanlog
      sdlog <- parameters$sdlog

      tf_meanlog <- fl(self$meanlog)
      tf_sdlog <- fl(self$sdlog)
      log_prob <- function(x) {
        # build distribution object
        # you will need to check that the name of the distribution exists
        # in tensorflow - this is just a simple starter helper, see
        # https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
        # for a list of distributions
        d <- tfp$distributions$lognormal(
          meanlog = meanlog,
          sdlog = sdlog
        )

        # If your distribution does not exist in Tensorflow probability, you
        # will need to write your own log prob - the density. See https://github.com/greta-dev/greta.distributions/blob/main/R/zero_inflated_negative_binomial.R for an example of this
      }

      sample <- function(seed) {
        # you will need to check that the name of the distribution exists
        # in tensorflow probability - this is just a simple starter helper
        # see https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
        # for a list of distributions
        d <- tfp$distributions$lognormal(
          meanlog = meanlog,
          sdlog = sdlog
        )
        continuous <- d$sample(seed = seed)
        tf$floor(continuous)

        # If your distribution does not exist in Tensorflow probability, you
        # will need to write your own sampler. See https://github.com/greta-dev/greta.distributions/blob/main/R/zero_inflated_negative_binomial.R for an example of this
      }

      list(
        log_prob = log_prob,
        sample = sample
      )
    }
  )
)

lognormal <- function(meanlog, sdlog, dim = NULL) {
  distrib(lognormal, meanlog, sdlog, dim)
}

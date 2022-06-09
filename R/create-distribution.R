build_greta_dist_definition <- function(dist_name){
  glue::glue(
  "{dist_name} <- R6Class(",
  "{dist_name},
    inherit = greta:::distribution_node,
    public = list(
      
    ")
}

build_greta_dist_init <- function(dist_name, 
                                  dist_arg_list){
  
  param_list <- glue::glue_collapse(x = dist_arg_list,
                                    sep = ", ")
  
  param_quoted_list <- glue::glue_collapse(
    x = glue::glue("'{dist_arg_list}'"),
    sep = ", "
    )
  
  greta_array_coerce <- glue::glue_collapse(
    x = glue::glue(
      "{dist_arg_list} <- as.greta_array({dist_arg_list})"
    ),
    sep = "\n"
  )
  
  greta_self_assign <- glue::glue_collapse(
    x = glue::glue(
      "self${dist_arg_list} <- {dist_arg_list}"
      ),
    sep = "\n"
  )
  
  greta_array_coerce_assign <- glue::glue_collapse(
    glue::glue("{greta_array_coerce}
                {greta_self_assign}"),
    sep = "\n"
  )
  
  greta_self_add <- glue::glue_collapse(
    x = glue::glue(
      "self$add_parameter({dist_arg_list}, 'dist_arg_list')"
    ),
    sep = "\n"
  )
  
  glue::glue(
    "initialize = function([param_list], \ndim) {
      
      [greta_array_coerce_assign]
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims([param_list], 
                                target_dim = dim)
      super$initialize('[dist_name]', dim, discrete = SET_OPTION)
      [greta_self_add]
    
    }
    
    ",
    .open = "[",
    .close = "]"
  )
}


build_greta_dist <- function(dist_name, dist_arg_list){
  
  
  build_definition <- build_greta_dist_definition(dist_name)
  build_initialize <- build_greta_dist_init(dist_name, dist_arg_list)
  build_tf_distrib <- build_greta_dist_tf_distrib(dist_name, dist_arg_list)
  build_log_prob <- build_greta_dist_log_prob(dist_name, dist_arg_list)
  build_sample <- build_greta_dist_sample(dist_name, dist_arg_list)
  build_r_func <- build_greta_dist_r(dist_name, dist_arg_list)
  
  glue::glue(
    build_definition,
    build_initialize,
    build_tf_distrib,
    build_log_prob,
    build_sample,
    build_r_func
  )
  
  glue::glue("
    initialize = function(PARAMETER1, PARAMETER2, dim) {
      
      PARAMETER1 <- as.greta_array(PARAMETER1)
      PARAMETER2 <- as.greta_array(PARAMETER2)
      
      self$PARAMETER1 <- PARAMETER1
      self$PARAMETER2 <- PARAMETER2
      
      # add the nodes as parents and parameters
      dim <- greta:::check_dims(PARAMETER1, 
                                PARAMETER1, 
                                target_dim = dim)
      super$initialize('{dist_name}', dim, discrete = SET_OPTION)
      self$add_parameter('PARAMETER1', 'PARAMETER2')
    
    tf_distrib = function(parameters, dag) {
      
      PARAMETER1 <- parameters$PARAMETER1
      PARAMETER2 <- parameters$PARAMETER2
      
      tf_PARAMETER1 <- fl(self$PARAMETER1)
      tf_PARAMETER2 <- fl(self$PARAMETER2)
      
      log_prob <- function(x) {
        
        # build distribution object
        # you will need to check that the name of the distribution exists
        # in tensorflow - this is just a simple starter helper
        # see https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
        # for a list of distributions
        d <- tfp$distributions${dist_name}(
          PARAMETER1 = PARAMETER1,
          PARAMETER2 = PARAMETER2
        )
        
        # This is where you'll need to write your own methods for computing
        # the density in tensorflow
        
      }
    sample <- function(seed) {
        
        # you will need to check that the name of the distribution exists
        # in tensorflow - this is just a simple starter helper
        # see https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
        # for a list of distributions
        d <- tfp$distributions${dist_name}(
          PARAMETER1 = PARAMETER1,
          PARAMETER2 = PARAMETER2
        )
        continuous <- d$sample(seed = seed)
        tf$floor(continuous)
        
      }
      
      list(log_prob = log_prob, sample = sample)
      
    }
  )
  
  {dist_name} <- function(PARAMETER1, PARAMETER2, dim = NULL) {
  greta:::distrib({dist_name}, PARAMETER1, PARAMETER2, dim)
}
")
}
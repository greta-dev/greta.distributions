build_greta_dist_definition <- function(dist_name, dist_arg_list){
  
  args <- glue::glue_collapse(
    x = glue::glue("{dist_arg_list} = NA,"),
    sep = "\n"
  )
  
  top <- glue::glue(
    '{dist_name} <- R6Class(
         classname = "{dist_name}",
         inherit = distribution_node,
         public = list(
           {args}'
    )
  
  top

}

build_greta_dist_init <- function(dist_name, 
                                  dist_arg_list){
  
  param_list <- glue::glue_collapse(x = dist_arg_list, sep = ", ")
  
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
      "self$add_parameter({dist_arg_list}, '{dist_arg_list}')"
    ),
    sep = "\n"
  )
  
  glue::glue(
    "initialize = function([param_list], \ndim) {
      
      [greta_array_coerce_assign]
      
      # add the nodes as parents and parameters
      dim <- check_dims([param_list], 
                                target_dim = dim)
      super$initialize('[dist_name]', dim, discrete = SET_OPTION)
      [greta_self_add]
    
    },
    
    ",
    .open = "[",
    .close = "]"
  )
}



  
build_greta_dist_tf_distrib <- function(dist_name, dist_arg_list){
  
  arg_param_assign <- glue::glue_collapse(
    glue::glue(
      "{dist_arg_list} <- parameters${dist_arg_list}"
    ),
    sep = "\n"
  )
  
  arg_tf_assign <- glue::glue_collapse(
    glue::glue(
      "tf_{dist_arg_list} <- fl(self${dist_arg_list})"
    ),
    sep = "\n"
  )
  
  glue::glue(
  'tf_distrib = function(parameters, dag) {
      
      [arg_param_assign]
      
      [arg_tf_assign]'
  ,
  .open = "[",
  .close = "]"
  )
}

build_greta_dist_log_prob <- function(dist_name, dist_arg_list){
  
  param_list <- glue::glue_collapse(
    x = glue::glue("{dist_arg_list} = {dist_arg_list}"),
    sep = ",\n"
  )
  
  glue::glue(
    "
    log_prob <- function(x) {
        
        # build distribution object
        # you will need to check that the name of the distribution exists
        # in tensorflow - this is just a simple starter helper, see 
        # https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
        # for a list of distributions
        d <- tfp$distributions$[dist_name](
           [param_list]
        )
        
        # This is where you'll need to write your own methods for computing
  # the density in tensorflow
    }
  
  ",
  .open = "[",
  .close = "]"
  )
  
}

build_greta_dist_sample <- function(dist_name, dist_arg_list){
  
  arg_list <- glue::glue_collapse(
    glue::glue(
      "{dist_arg_list} = {dist_arg_list}"
    ),
    sep = ", \n"
  )
  
  
  glue::glue(
  "
  
  sample <- function(seed) {
    
    # you will need to check that the name of the distribution exists
    # in tensorflow - this is just a simple starter helper
    # see https://www.tensorflow.org/probability/api_docs/python/tfp/distributions
    # for a list of distributions
    d <- tfp$distributions$[dist_name](
      [arg_list]
    )
    continuous <- d$sample(seed = seed)
    tf$floor(continuous)
    
  }
  
  list(log_prob = log_prob, sample = sample)
  
  }
  ) 
    )
  "
  ,
  .open = "[",
  .close = "]"
  )
    
}
build_greta_dist_r <- function(dist_name, dist_arg_list){
  arg_list <- glue::glue_collapse(dist_arg_list, sep = ", ")
  
  glue::glue(
    "

[dist_name] <- function([arg_list], dim = NULL) {
  distrib([dist_name], [arg_list], dim)
}
  "
  ,
  .open = "[",
  .close = "]"
  )
}

#' Write out a template greta distribution
#'
#' @param dist_name character length 1, name of the distribution
#' @param dist_arg_list character vector, arguments to the distribution
#'
#' @return text of a basic greta distribution
#' @export
#'
#' @examples
#' greta_distribution_template(
#'   dist_name = "lognormal", 
#'   dist_arg_list = c("meanlog", "sdlog")
#'   )
greta_distribution_template <- function(dist_name, dist_arg_list){
  
  
  build_definition <- build_greta_dist_definition(dist_name, dist_arg_list)
  build_initialize <- build_greta_dist_init(dist_name, dist_arg_list)
  build_tf_distrib <- build_greta_dist_tf_distrib(dist_name, dist_arg_list)
  build_log_prob <- build_greta_dist_log_prob(dist_name, dist_arg_list)
  build_sample <- build_greta_dist_sample(dist_name, dist_arg_list)
  build_r_func <- build_greta_dist_r(dist_name, dist_arg_list)
  
  distribution_txt <- glue::glue(
    "{build_definition}",
    "\n{build_initialize}",
    "{build_tf_distrib}",
    "\n{build_log_prob}",
    "{build_sample}",
    "\n{build_r_func}"
  )
  
  styler::style_text(distribution_txt)
  
}
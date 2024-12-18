greta_distribution_template_test <- function(dist_name){
  template <- '
test_that("[dist_name] distribution has correct density", {
  skip_if_not(check_tf_version())
  
  param_1 <- 2
  param_2 <- 0.1
  compare_distribution(
    greta_fun = [dist_name],
    r_fun = TODO_YOUR_RANDOM_SAMPLING_FUNCTION(),
    # e.g., rnorm, extraDist::...
    # see for example:
    parameters = list(param_1 = param_1, param_2 = param_2),
    x = extraDist::rzip(
      n = 100,
      lambda = param_1,
      pi = param_2
    )
  )
})
'
  templated_test <- glue::glue(
    template,
    .open = "[",
    .close = "]"
    )
  # test test file exists...
  
  styler::style_text(templated_test)
}


write_distribution_test <- function(dist_name, overwrite = FALSE){
  template_test_info <- greta_distribution_template_test(dist_name)
  
  dist_test_path <- make_test_path(dist_name)
  
  check_if_overwrite(dist_name, overwrite, dist_test_path)
  
  cli::cli_inform(
    message = c(
      "i" = "Writing new test template for distribution, \\
      {.fun dist_name}, to {.file {dist_test_path}}."
    )
  )
  
  #TODO 
  # check directory exists
  file.create(dist_test_path)
  writeLines(text = template_test_info,
             con = dist_test_path)
  
  cli::cli_inform(
    message = c(
      "i" = "See {.help write_distribution_test} for details"
    )
  )
  
}
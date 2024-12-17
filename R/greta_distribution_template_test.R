greta_distribution_template_test <- function(dist_name){
  template <- '
test_that("[dist_name] distribution has correct density", {
  skip_if_not(check_tf_version())
  
  compare_distribution(
    greta_fun = [dist_name],
    r_fun = TODO_YOUR_RANDOM_SAMPLING_FUNCTION(),
    # e.g., rnorm, extraDist::...
    # see for example:
    parameters = list(lambda = 2, pi = 0.2),
    x = extraDist::rzip(
      n = 100,
      lambda = 2,
      pi = 0.2
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


write_new_distribution_test <- function(dist_name){
  template_test_info <- greta_distribution_template_test(dist_name)
  
  dist_test_path <- paste0(
    file.path(
      "tests", 
      "testthat",
      dist_name
      ),
    ".R"
    )
  
  cli::cli_inform(
    message = c(
      "i" = "Writing new test template for distribution, {.fun dist_name}, to file, {.file {dist_test_path}}"
    )
  )
  
  #TODO 
  # check directory exists
  file.create(dist_test_path)
  writeLines(text = template_test_info,
             con = dist_test_path)
  
  cli::cli_inform(
    message = c(
      "i" = "See {.help greta_distribution_template_test} for details",
    )
  )
  
}
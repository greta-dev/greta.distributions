#' Create a test template for a distribution
#' 
#' When you add a new distribution, you want to add a test to make sure it is
#'   behaving as expected. This function generates a test template. It only 
#'   creates the text, it does not write to file. See 
#'   `greta_dist_write()` to write the test to file automatically.
#'
#' @param dist_name character. A distribution name. E.g., "lognormal"
#'
#' @returns text containing test template code
#'
#' @examples
#' gumbel <- greta_dist_template_test("gumbel")
#' @export
greta_dist_template_test <- function(dist_name){
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

test_that("[dist_name] distribution works with calculate", {
  skip_if_not(check_tf_version())
  
  param_1 <- 2
  param_2 <- 0.1
  example_[dist_name] <- [dist_name](param_1, param_2)
  
  expect_no_error(calculate(example_[dist_name], nsim = 10))
  expect_no_error(m <- model(example_[dist_name]))
  expect_no_error(mcmc(m, n_samples = 10, warmup = 10))
    
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


#' Create a test template for a distribution
#' 
#' When you add a new distribution, you want to add a test to make sure it is
#'   behaving as expected. This function generates a test template. See also 
#'  `greta_dist_template_test()` to see the text generated if you want 
#'  to save it somewhere else.
#'
#' @param dist_name character, name of distribution.
#' @param overwrite logical. Default is FALSE. Whether to overwrite the test 
#'   file if it already exists.
#'
#' @returns test file written out to `tests/testthat/test-{dist_name}`.
#' @export
#'
#' @examples
#' \dontrun{
#' greta_dist_write("gumbel")
#' }
greta_dist_write_test <- function(dist_name, overwrite = FALSE){
  template_test_info <- greta_dist_template_test(dist_name)
  
  dist_test_path <- make_test_path(dist_name)
  
  check_if_overwrite(dist_name, overwrite, dist_test_path)
  
  cli::cli_inform(
    message = c(
      "i" = "Writing new test template for distribution, \\
      {.fun dist_name}, to {.file {dist_test_path}}."
    )
  )
  
  file.create(dist_test_path)
  writeLines(text = template_test_info,
             con = dist_test_path)
  
  cli::cli_inform(
    message = c(
      "i" = "See {.help greta_dist_write} for details"
    )
  )
  
}

#' Write a new greta distribution template file
#'
#' This generates a starting place for writing a new `greta` distribution.
#'   By default this will save the output to "R/dist_name.R". To save to
#'   somewhere else, see `greta_distribution_template()`, which will generate
#'   the R code as plain text, and you can then save somewhere else as you wish.
#'
#' @param dist_name name of distribution.
#' @param dist_arg_list arguments for distribution.
#' @param overwrite logical. default FALSE. Whether to overwrite the test
#'   file if it already exists.
#'
#' @returns writes to file a new distribution name.
#'
#' @examples
#' \dontrun{
#' write_new_distribution(
#'   dist_name = "lognormal",
#'   dist_arg_list = c("meanlog", "sdlog")
#'   )
#' }
#' @export
write_new_distribution <- function(
  dist_name = NULL,
  dist_arg_list = NULL,
  overwrite = FALSE
) {
  template_info <- greta_distribution_template(
    dist_name = dist_name,
    dist_arg_list = dist_arg_list
  )

  dist_path <- make_r_path(dist_name)

  check_if_overwrite(dist_name, overwrite, dist_path)

  cli::cli_inform(
    message = c(
      "i" = "Writing new distribution, {.fun {dist_name}}, to file, \\
      {.file {dist_path}}"
    )
  )

  #TODO
  # check directory exists
  file.create(dist_path)
  writeLines(text = template_info, con = dist_path)

  cli::cli_inform(
    message = c(
      "i" = "Make changes to file, {.file {dist_path}} in order to get \\
      distribution working in {.pkg greta}.",
      "i" = "See {.help create_new_distribution} for details",
      "i" = "Add a test template for {.fun {dist_name}} with {.code write_distribution_test({.val {dist_name}})}"
    )
  )
}

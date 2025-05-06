create_a_directory <- function(path) {
  if (dir.exists(path)) {
    return(invisible(FALSE))
  }
  dir.create(path, recursive = TRUE)
  invisible(TRUE)
}

make_r_path <- function(dist_name) {
  create_a_directory("R")
  dist_path1 <- glue::glue("{file.path('R', 'dist-{dist_name}')}.R")
  dist_path2 <- glue::glue(dist_path1)
  dist_path2
}

make_test_path <- function(dist_name) {
  create_a_directory("tests/testthat")
  dist_test_path <- file.path(
    "tests",
    "testthat",
    glue::glue("test-dist-{dist_name}.R")
  )
  dist_test_path
}

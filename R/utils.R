create_a_directory <- function(path){
  if (dir.exists(path)) {
    return(invisible(FALSE))
  }
  dir.create(path, recurse = TRUE)
  invisible(TRUE)
}

make_r_path <- function(dist_name){
  create_a_directory("R")
  dist_path <- paste0(file.path("R", dist_name),".R")
  dist_path
}

make_test_path <- function(dist_name){
  create_a_directory("tests/testthat")
  dist_test_path <- file.path(
    "tests", 
    "testthat",
    glue::glue("test_{dist_name}.R")
  )
  dist_test_path
}



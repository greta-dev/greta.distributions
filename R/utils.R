
n_dim <- function(x) length(dim(x))

check_if_2d_array <- function(x){
  if (n_dim(x) != 2) {
    msg <- cli::format_error(
      c(
        "{.var x} must be a 2D array",
        "but {.var x} has dimensions {paste(dim(x), collapse = 'x')}"
      )
    ) 
    stop(
      msg,
      call. = FALSE
    )
  }
}

check_n_col_gte <- function(x, n_col){
  if (ncol(x) < n_col) {
    msg <- cli::format_error(
      c(
        "{.var x} must have at least two columns",
        "but {.var x} has {ncol(x)} columns"
      )
    ) 
    stop(
      msg,
      call. = FALSE
    )
  }
}

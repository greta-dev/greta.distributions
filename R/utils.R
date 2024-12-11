
n_dim <- function(x) length(dim(x))

check_if_2d_array <- function(x,
                              call = rlang::caller_env()){
  if (n_dim(x) != 2) {
    cli::cli_abort(
      c(
        "{.var x} must be a 2D array",
        "but {.var x} has dimensions {paste(dim(x), collapse = 'x')}"
      ),
      call = call
    ) 
  }
}

check_n_col_gte <- function(x,
                            n_col,
                            call = rlang::caller_env()){
  if (ncol(x) < n_col) {
    cli::cli_abort(
      c(
        "{.var x} must have at least two columns",
        "but {.var x} has {ncol(x)} columns"
      ),
      call = call
    ) 
  }
}

check_valid_probability <- function(x, 
                                    var_name = "x",
                                    call = rlang::caller_env()) {
  if (any(x < 0) | any(x > 1)) {
    
    first_line <- glue::glue(
      "{.var [var_name]} must be a valid probability - between 0 and 1",
      .open = "[",
      .close = "]"
    )
    second_line <- glue::glue(
      "We see {.var [var_name]} = {x}",
      .open = "[",
      .close = "]"
    )
    cli::cli_abort(
      c(
        first_line,
        second_line
      ),
      call = call
    )
  }
}

check_if_2d_gte_two_col <- function(p,
                                 call = rlang::caller_env()){
  does_not_have_at_least_two_cols <- ncol(p) < 2 | length(dim(p)) != 2
  if (does_not_have_at_least_two_cols) {
    cli::cli_abort(
      c(
        "{.var p} must be a 2D array with at least two columns",
        "but {.var p} has dimensions {paste(dim(p), collapse = 'x')}"
      ),
      call = call
    ) 
  }
}

check_if_2d_one_col <- function(psi,
                                call = rlang::caller_env()){
  # check dimensions of psi
  not_2d_or_one_col <- ncol(psi) != 1 | length(dim(psi)) != 2
  if (not_2d_or_one_col) {
    cli::cli_abort(
      c(
        "{.var psi} must be a 2D array with one column",
        "but {.var psi} has dimensions {paste(dim(psi), collapse = 'x')}"
      ),
      call = call
    )
  }
}

check_params_same_rows <- function(p, 
                                   psi,
                                   call = rlang::caller_env()){
  dim_p <- nrow(p)
  dim_psi <- nrow(psi)
  
  not_same_nrows <- dim_p != dim_psi
  
  if (not_same_nrows) {
    cli::cli_abort(
      c(
        "{.var p} and {.var psi} must have the same number of rows",
        "But we see {.var p} and {.var psi} have:",
        "{.var p}: {dim_p} {?row/rows}",
        "{.var psi}: {dim_psi} {?row/rows}",
        "Perhaps you need to coerce {.var p} or {.var psi} to an \\
            appropriate matrix?"
      ),
      call = call
    )
  }
}

check_dim_positive_scalar_int <- function(dim,
                                          call = rlang::caller_env()){
  # check dim is a positive scalar integer
  dim_old <- dim
  dim <- as.integer(dim)
  not_scalar_positive_integer <- length(dim) > 1 || dim <= 0 || !is.finite(dim)
  if (not_scalar_positive_integer) {
    cli::cli_abort(
      c(
        "{.var dim} must be a scalar positive integer, but was:",
        "{dim_old}"
      ),
      call = call
    )
  }
}

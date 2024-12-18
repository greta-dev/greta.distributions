check_if_null <- function(x, 
                          msg = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()){
  not_specified <- !is.character(x) || is.null(x)
  if (not_specified) {
    cli::cli_abort(
      "{.arg {arg}} must be specified",
      msg
    )
  }
}
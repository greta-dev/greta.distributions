check_if_null <- function(x, 
                          msg = NULL,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()){
  not_specified <- !is.character(x) || is.null(x)
  if (not_specified) {
    cli::cli_abort(
     message = c("{.arg {arg}} must be specified",
                 msg),
     call = call
    )
  }
}

check_if_overwrite <- function(dist_name, 
                               overwrite, 
                               dist_path,
                               call = rlang::caller_env()){
  
  path_already_exists <- file.exists(dist_path)
  error_user <- path_already_exists && !overwrite
  
  if (error_user){
    cli::cli_abort(
      message = c(
        "Distribution name file already exists: {.path {dist_path}} \\
        and {.arg overwrite} is FALSE.",
        "Perhaps you meant to set {.arg overwrite} to TRUE?"
      ),
      call = call
    )
  }
}

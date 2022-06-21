check_valid_probability <- function(x, var_name = "x") {
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
    msg <- cli::format_error(
      c(
        first_line,
        second_line
      )
    )
    stop(msg,
         call. = FALSE)
  }
}
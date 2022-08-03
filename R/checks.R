# check functions


check_break_length <- function(breaks){
  if (length(breaks) <= 1) {
    msg <- cli::format_error(
      c(
        "{.var breaks} must be a vector with at least two break points",
        "but {.var breaks} has length {length(breaks)}"
      )
    ) 
    stop(
      msg,
      call. = FALSE
    )
  }
}
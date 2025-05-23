% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_distribution.R
\name{write_new_distribution}
\alias{write_new_distribution}
\title{Write a new greta distribution template file}
\usage{
write_new_distribution(
  dist_name = NULL,
  dist_arg_list = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{dist_name}{name of distribution.}

\item{dist_arg_list}{arguments for distribution.}

\item{overwrite}{logical. default FALSE. Whether to overwrite the test
file if it already exists.}
}
\value{
writes to file a new distribution name.
}
\description{
This generates a starting place for writing a new \code{greta} distribution.
By default this will save the output to "R/dist_name.R". To save to
somewhere else, see \code{greta_distribution_template()}, which will generate
the R code as plain text, and you can then save somewhere else as you wish.
}
\examples{
\dontrun{
write_new_distribution(
  dist_name = "lognormal",
  dist_arg_list = c("meanlog", "sdlog")
  )
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zero_inflated_negative_binomial.R
\name{zero_inflated_negative_binomial}
\alias{zero_inflated_negative_binomial}
\title{Zero Inflated Negative Binomial}
\usage{
zero_inflated_negative_binomial(size, prob, pi, dim = NULL)
}
\arguments{
\item{size}{positive integer parameter}

\item{prob}{probability parameter (\verb{0 < prob < 1}),}

\item{pi}{proportion of zeros}

\item{dim}{a scalar giving the number of rows in the resulting greta array}
}
\description{
A Zero Inflated Negative Binomial distribution
}
\examples{
\dontrun{
zinb <- zero_inflated_negative_binomial(size = 2, prob= 0.2, pi = 0.10)
calculate(zinb, nsim = 10)
m <- model(zinb)
mcmc(m)
}
}

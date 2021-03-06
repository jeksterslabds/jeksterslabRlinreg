% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci.R
\name{ci}
\alias{ci}
\title{Confidence Intervals of Estimates of Regression Coefficients}
\usage{
ci(betahat, sehatbetahat, n, k)
}
\arguments{
\item{betahat}{Numeric vector of length \code{k} or \code{k} by \code{1} matrix.
The vector \eqn{\boldsymbol{\hat{\beta}}} is a \eqn{k \times 1} vector of estimates
of \eqn{k} unknown regression coefficients.}

\item{sehatbetahat}{Numeric vector.
Standard errors of regression coefficients.}

\item{n}{Integer.
Sample size.}

\item{k}{Integer.
Number of regressors including a regressor
whose value is 1 for each observation on the first column.}
}
\value{
Returns a matrix with the following columns
\describe{
\item{ci_0.05}{Lower limit 99.99\% confidence interval.}
\item{ci_0.5}{Lower limit 99\% confidence interval.}
\item{ci_2.5}{Lower limit 95\% confidence interval.}
\item{ci_97.5}{Upper limit 95\% confidence interval.}
\item{ci_99.5}{Upper limit 99\% confidence interval.}
\item{ci_99.95}{Upper limit 99.99\% confidence interval.}
}
}
\description{
Confidence Intervals of Estimates of Regression Coefficients
}
\seealso{
Other hypothesis testing functions: 
\code{\link{.anovatable}()},
\code{\link{anovatable}()},
\code{\link{nhst}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{hypothesis testing functions}
\keyword{inference}

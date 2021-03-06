% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nhst.R
\name{nhst}
\alias{nhst}
\title{Hypothesis Test for Estimates of Regression Coefficients}
\usage{
nhst(betahat, sehatbetahat, n, k)
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
\item{coef}{Coefficients.}
\item{se}{Standard error.}
\item{t}{t-statistic.}
\item{p}{p-value.}
}
}
\description{
Hypothesis Test for Estimates of Regression Coefficients
}
\seealso{
Other hypothesis testing functions: 
\code{\link{.anovatable}()},
\code{\link{anovatable}()},
\code{\link{ci}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{hypothesis testing functions}
\keyword{inference}

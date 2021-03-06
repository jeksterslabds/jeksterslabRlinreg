% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcovhat.R
\name{vcovhatbetahat}
\alias{vcovhatbetahat}
\title{Variance-Covariance Matrix of Estimates of Regression Coefficients}
\usage{
vcovhatbetahat(X, y)
}
\arguments{
\item{X}{\code{n} by \code{k} numeric matrix.
The data matrix \eqn{\mathbf{X}}
(also known as design matrix, model matrix or regressor matrix)
is an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
which includes a regressor whose value is 1 for each observation on the first column.}

\item{y}{Numeric vector of length \code{n} or \code{n} by \code{1} matrix.
The vector \eqn{\mathbf{y}} is an \eqn{n \times 1} vector of observations
on the regressand variable.}
}
\value{
Returns the variance-covariance matrix
of estimates of regression coefficients.
}
\description{
Calculates the variance-covariance matrix of estimates of regression coefficients using
\deqn{
    \widehat{\mathrm{cov}} \left( \boldsymbol{\hat{\beta}} \right) =
    \hat{\sigma}_{\varepsilon}^2 \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
  }
where \eqn{\hat{\sigma}_{\varepsilon}^{2}}
is the estimate of the error variance \eqn{\sigma_{\varepsilon}^{2}}
and \eqn{\mathbf{X}} is the data matrix, that is,
an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
which includes a regressor whose value is 1 for each observation on the first column.
}
\examples{
# Simple regression------------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, c(1, ncol(X))]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
vcovhatbetahat(X = X, y = y)

# Multiple regression----------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
vcovhatbetahat(X = X, y = y)
}
\references{
\href{https://en.wikipedia.org/wiki/Linear_regression}{Wikipedia: Linear Regression}

\href{https://en.wikipedia.org/wiki/Ordinary_least_squares}{Wikipedia: Ordinary Least Squares}
}
\seealso{
Other variance-covariance of estimates of regression coefficients functions: 
\code{\link{.vcovhatbetahatbiased}()},
\code{\link{.vcovhatbetahat}()},
\code{\link{vcovhatbetahatbiased}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{variance-covariance of estimates of regression coefficients functions}
\keyword{vcov}

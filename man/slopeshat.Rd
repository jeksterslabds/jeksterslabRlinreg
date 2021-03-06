% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betahat_matrix.R
\name{slopeshat}
\alias{slopeshat}
\title{Estimates of Regression Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}}
\usage{
slopeshat(X, y)
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
Returns the estimated slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
of a linear regression model derived from the estimated variance-covariance matrix.
}
\description{
Estimates of Regression Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
}
\details{
Estimates of the linear regression slopes are calculated using
\deqn{
    \boldsymbol{\hat{\beta}}_{2, \cdots, k} =
    \boldsymbol{\hat{\Sigma}}_{\mathbf{X}}^{T} \boldsymbol{\hat{\sigma}}_{\mathbf{y}, \mathbf{X}}
  }

where
\itemize{
\item \eqn{\boldsymbol{\hat{\Sigma}}_{\mathbf{X}}}
is the \eqn{p \times p} covariance matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k} and
\item \eqn{\boldsymbol{\hat{\sigma}}_{\mathbf{y}, \mathbf{X}}}
is the \eqn{p \times 1} column vector
of the covariances between the regressand \eqn{y} variable
and regressor variables \eqn{X_2, X_3, \cdots, X_k}
}
}
\examples{
# Simple regression------------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, c(1, ncol(X))]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
slopeshat(X = X, y = y)

# Multiple regression----------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
slopeshat(X = X, y = y)
}
\seealso{
Other beta-hat functions: 
\code{\link{.betahatnorm}()},
\code{\link{.betahatqr}()},
\code{\link{.betahatsvd}()},
\code{\link{.intercepthat}()},
\code{\link{.slopeshatprime}()},
\code{\link{.slopeshat}()},
\code{\link{betahat}()},
\code{\link{intercepthat}()},
\code{\link{slopeshatprime}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{beta-hat functions}
\keyword{beta-hat-ols}

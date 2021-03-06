% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betahatinference.R
\name{slopeshatprimeinference}
\alias{slopeshatprimeinference}
\title{Standardized Regression Slopes Hypothesis Test and Confidence Intervals}
\usage{
slopeshatprimeinference(X, y, sehatslopeshatprimetype = "textbook")
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

\item{sehatslopeshatprimetype}{Character string.
Standard errors for standardized regression slopes hypothesis test.
Options are \code{sehatslopeshatprimetype = "textbook"} and \code{sehatslopeshatprimetype = "delta"}.}
}
\description{
Standardized Regression Slopes Hypothesis Test and Confidence Intervals
}
\examples{
# Simple regression------------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, c(1, ncol(X))]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
slopeshatprimeinference(X = X, y = y)

# Multiple regression----------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
slopeshatprimeinference(X = X, y = y)
}
\seealso{
Other inference functions: 
\code{\link{.betahatinference}()},
\code{\link{.slopeshatprimeinference}()},
\code{\link{betahatinference}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{inference functions}
\keyword{inference}

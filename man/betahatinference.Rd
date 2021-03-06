% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betahatinference.R
\name{betahatinference}
\alias{betahatinference}
\title{Regression Coefficients Hypothesis Test and Confidence Intervals}
\usage{
betahatinference(X, y)
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
Returns a matrix with the following columns
\describe{
\item{coef}{Coefficients.}
\item{se}{Standard error.}
\item{t}{t-statistic.}
\item{p}{p-value.}
\item{ci_0.05}{Lower limit 99.99\% confidence interval.}
\item{ci_0.5}{Lower limit 99\% confidence interval.}
\item{ci_2.5}{Lower limit 95\% confidence interval.}
\item{ci_97.5}{Upper limit 95\% confidence interval.}
\item{ci_99.5}{Upper limit 99\% confidence interval.}
\item{ci_99.95}{Upper limit 99.99\% confidence interval.}
}
}
\description{
Regression Coefficients Hypothesis Test and Confidence Intervals
}
\examples{
# Simple regression------------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, c(1, ncol(X))]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
betahatinference(X = X, y = y)

# Multiple regression----------------------------------------------
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
# age is removed
X <- X[, -ncol(X)]
betahatinference(X = X, y = y)
}
\seealso{
Other inference functions: 
\code{\link{.betahatinference}()},
\code{\link{.slopeshatprimeinference}()},
\code{\link{slopeshatprimeinference}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{inference functions}
\keyword{inference}

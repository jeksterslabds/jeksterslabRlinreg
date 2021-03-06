% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta.R
\name{intercept}
\alias{intercept}
\title{Regression Intercept \eqn{\beta_{1}}}
\usage{
intercept(X, y)
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
Returns the intercept \eqn{\beta_1}
of a linear regression model derived from the means
and the slopes \eqn{\left( \boldsymbol{\beta}_{2, \cdots, k} \right)} .
}
\description{
Derives the intercept \eqn{\beta_1} of a linear regression model
from the \eqn{p \times 1} regression slopes
\eqn{\left( \boldsymbol{\beta}_{2, \cdots, k} \right)},
the mean of the regressand \eqn{\left( \mu_y \right)},
and the \eqn{p \times 1} means of regressors \eqn{{X}_{2}, {X}_{3}, \dots, {X}_{k}}
\eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
}
\details{
The intercept \eqn{\beta_1} is given by
\deqn{
    \beta_1 = \mu_y - \boldsymbol{\mu}_{\mathbf{X}}
    \boldsymbol{\beta}_{2, \cdots, k}^{T} .
  }
}
\seealso{
Other parameter functions: 
\code{\link{.intercept}()},
\code{\link{.slopesprime}()},
\code{\link{.slopes}()},
\code{\link{sigma2epsilon}()},
\code{\link{slopesprime}()},
\code{\link{slopes}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{parameter functions}
\keyword{parameter}

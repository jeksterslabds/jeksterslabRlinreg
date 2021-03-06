% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epsilonhat.R
\name{.yminusyhat}
\alias{.yminusyhat}
\title{Residuals
\eqn{
    \left( \boldsymbol{\hat{\varepsilon}}
    = \mathbf{y} - \mathbf{\hat{y}} \right)
  }}
\usage{
.yminusyhat(y, yhat = NULL, X = NULL, betahat = NULL)
}
\arguments{
\item{y}{Numeric vector of length \code{n} or \code{n} by \code{1} matrix.
The vector \eqn{\mathbf{y}} is an \eqn{n \times 1} vector of observations
on the regressand variable.}

\item{yhat}{Numeric vector of length \code{n} or \code{n} by \code{1} numeric matrix.
\eqn{n \times 1} vector of predicted values of \eqn{\mathbf{y}}
\eqn{\left( \mathbf{\hat{y}} \right)}.}

\item{X}{\code{n} by \code{k} numeric matrix.
The data matrix \eqn{\mathbf{X}}
(also known as design matrix, model matrix or regressor matrix)
is an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
which includes a regressor whose value is 1 for each observation on the first column.}

\item{betahat}{Numeric vector of length \code{k} or \code{k} by \code{1} matrix.
The vector \eqn{\boldsymbol{\hat{\beta}}} is a \eqn{k \times 1} vector of estimates
of \eqn{k} unknown regression coefficients.}
}
\value{
Returns an \eqn{n \times 1} matrix of residuals
\eqn{\left( \boldsymbol{\hat{\varepsilon}} \right)},
that is, the difference between the observed \eqn{\left( \mathbf{y} \right)}
and predicted \eqn{\left( \mathbf{\hat{y}} \right)} values of the regressand variable
\eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}} \right)}.
}
\description{
Calculates residuals using
\deqn{
    \hat{\varepsilon}_{i}
    = Y_{i} - \hat{Y}_{i} \\
    = Y_{i} - \left( \hat{\beta}_{1} + \hat{\beta}_{2} X_{2i} +
      \hat{\beta}_{3} X_{3i} + \dots + \hat{\beta}_{k} X_{ki} \right) \\
    = Y_{i} - \hat{\beta}_{1} - \hat{\beta}_{2} X_{2i} -
      \hat{\beta}_{3} X_{3i} - \dots - \hat{\beta}_{k} X_{ki} .
  }
In matrix form
\deqn{
    \boldsymbol{\hat{\varepsilon}}
    = \mathbf{y} - \mathbf{\hat{y}} \\
    = \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} .
  }
}
\details{
If \code{yhat = NULL}, the \code{yhat} vector is computed using \code{\link[=Xbetahat]{Xbetahat()}}
with \code{X} as a required argument and \code{betahat} as an optional argument.
If \code{yhat} is provided, \code{X} and \code{betahat} are not needed.
}
\references{
\href{https://en.wikipedia.org/wiki/Errors_and_residuals}{Wikipedia: Errors and Residuals}
}
\seealso{
Other residuals functions: 
\code{\link{.My}()},
\code{\link{.tepsilonhat}()},
\code{\link{My}()},
\code{\link{epsilonhat}()},
\code{\link{tepsilonhat}()},
\code{\link{yminusyhat}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{residuals functions}
\keyword{residuals}

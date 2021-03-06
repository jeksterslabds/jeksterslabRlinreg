% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SS.R
\name{.RSS}
\alias{.RSS}
\title{Residual Sum of Square (from \eqn{\boldsymbol{\hat{\varepsilon}}})}
\usage{
.RSS(epsilonhat = NULL, X, y, betahat = NULL)
}
\arguments{
\item{epsilonhat}{Numeric vector of length \code{n} or \code{n} by 1 numeric matrix.
\eqn{n \times 1} vector of residuals.}

\item{X}{\code{n} by \code{k} numeric matrix.
The data matrix \eqn{\mathbf{X}}
(also known as design matrix, model matrix or regressor matrix)
is an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
which includes a regressor whose value is 1 for each observation on the first column.}

\item{y}{Numeric vector of length \code{n} or \code{n} by \code{1} matrix.
The vector \eqn{\mathbf{y}} is an \eqn{n \times 1} vector of observations
on the regressand variable.}

\item{betahat}{Numeric vector of length \code{k} or \code{k} by \code{1} matrix.
The vector \eqn{\boldsymbol{\hat{\beta}}} is a \eqn{k \times 1} vector of estimates
of \eqn{k} unknown regression coefficients.}
}
\value{
Returns residual sum of squares \eqn{\left( \mathrm{RSS} \right)}.
}
\description{
Calculates the residual sum of squares \eqn{\left( \mathrm{RSS} \right)} using
\deqn{
    \mathrm{RSS} = \sum_{i = 1}^{n} \left( Y_i - \hat{Y}_i \right)^2 \\
    = \sum_{i = 1}^{n} \left( Y_i -
    \left[ \hat{\beta}_{1} + \hat{\beta}_{2} X_{2i} + \hat{\beta}_{3} X_{3i} +
    \dots + \hat{\beta}_{k} X_{ki} \right] \right)^2 \\
    = \sum_{i = 1}^{n} \left( Y_i - \hat{\beta}_{1} - \hat{\beta}_{2} X_{2i}
    - \hat{\beta}_{3} X_{3i} - \dots - \hat{\beta}_{k} X_{ki} \right)^2 .
  }
In matrix form
\deqn{
    \mathrm{RSS} = \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2} \\
    = \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2} \\
    = \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{\prime}
    \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right) .
  }
Or simply
\deqn{
    \mathrm{RSS} = \sum_{i = 1}^{n} \boldsymbol{\hat{\varepsilon}}_{i}^{2}
    = \boldsymbol{\hat{\varepsilon}}^{\prime} \boldsymbol{\hat{\varepsilon}}
  }
where
\eqn{\boldsymbol{\hat{\varepsilon}}} is an \eqn{n \times 1} vector of residuals,
that is, the difference between the observed and predicted value of \eqn{\mathbf{y}}
\eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}} \right)}.
Equivalent computational matrix formula
\deqn{
    \mathrm{RSS} = \mathbf{y}^{\prime} \mathbf{y} - 2 \boldsymbol{\hat{\beta}} \mathbf{X}^{\prime}
    \mathbf{y} + \boldsymbol{\hat{\beta}}^{\prime} \mathbf{X}^{\prime} \mathbf{X}
    \boldsymbol{\hat{\beta}}.
  }
Note that
\deqn{
    \mathrm{TSS} = \mathrm{ESS} + \mathrm{RSS}.
  }
}
\details{
If \code{epsilonhat = NULL}, \eqn{\left( \mathrm{RSS} \right)} is computed with \code{X} and \code{y} as required arguments
and \code{betahat} as an optional argument.
}
\references{
\href{https://en.wikipedia.org/wiki/Residual_sum_of_squares}{Wikipedia: Residual Sum of Squares}

\href{https://en.wikipedia.org/wiki/Explained_sum_of_squares}{Wikipedia: Explained Sum of Squares}

\href{https://en.wikipedia.org/wiki/Total_sum_of_squares}{Wikipedia: Total Sum of Squares}

\href{https://en.wikipedia.org/wiki/Coefficient_of_determination}{Wikipedia: Coefficient of Determination}
}
\seealso{
Other sum of squares functions: 
\code{\link{.ESS}()},
\code{\link{ESS}()},
\code{\link{RSS}()},
\code{\link{TSS}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{sum of squares functions}
\keyword{SS}

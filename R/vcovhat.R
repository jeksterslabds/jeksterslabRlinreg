#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon}^{2}})
#'
#' @description Calculates the variance-covariance matrix of estimates of regression coefficients using
#'   \deqn{
#'     \widehat{\mathrm{cov}} \left( \boldsymbol{\hat{\beta}} \right) =
#'     \hat{\sigma}_{\varepsilon}^2 \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'   }
#'   where \eqn{\hat{\sigma}_{\varepsilon}^{2}}
#'   is the estimate of the error variance \eqn{\sigma_{\varepsilon}^{2}}
#'   and \eqn{\mathbf{X}} is the data matrix, that is,
#'   an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
#'   which includes a regressor whose value is 1 for each observation on the first column.
#'
#' @details If `sigma2hatepsilonhat = NULL`, `sigma2hatepsilonhat` is computed
#'   using [`sigma2hatepsilonhat()`].
#'
#' @family variance-covariance of estimates of regression coefficients functions
#' @keywords inference
#' @param sigma2hatepsilonhat Numeric.
#'   Estimate of error variance.
#' @inheritParams sigma2hatepsilonhat
#' @return Returns the variance-covariance matrix
#'   of estimates of regression coefficients.
#' @inherit sigma2hatepsilonhat references
#' @export
.vcovhatbetahat <- function(sigma2hatepsilonhat = NULL,
                            X,
                            y) {
  if (is.null(sigma2hatepsilonhat)) {
    sigma2hatepsilonhat <- sigma2hatepsilonhat(
      X = X,
      y = y
    )
  }
  unname(
    sigma2hatepsilonhat * solve(crossprod(X))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family variance-covariance of estimates of regression coefficients functions
#' @keywords inference
#' @param sigma2hatepsilonhatbiased Numeric.
#'   Biased estimate of error variance.
#' @inheritParams sigma2hatepsilonhatbiased
#' @inherit sigma2hatepsilonhatbiased references
#' @export
.vcovhatbetahatbiased <- function(sigma2hatepsilonhatbiased = NULL,
                                  X,
                                  y) {
  if (is.null(sigma2hatepsilonhatbiased)) {
    sigma2hatepsilonhatbiased <- sigma2hatepsilonhatbiased(
      X = X,
      y = y
    )
  }
  unname(
    sigma2hatepsilonhatbiased * solve(crossprod(X))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients
#'
#' @family variance-covariance of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .vcovhatbetahat
#' @inherit .vcovhatbetahat return description references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' vcovhatbetahat(
#'   X = X,
#'   y = y
#' )
#' @export
vcovhatbetahat <- function(X,
                           y) {
  .vcovhatbetahat(
    sigma2hatepsilonhat = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family variance-covariance of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .vcovhatbetahatbiased
#' @inherit .vcovhatbetahatbiased return description references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' vcovhatbetahatbiased(
#'   X = X,
#'   y = y
#' )
#' @export
vcovhatbetahatbiased <- function(X,
                                 y) {
  .vcovhatbetahatbiased(
    sigma2hatepsilonhatbiased = NULL,
    X = X,
    y = y
  )
}

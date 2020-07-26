#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon}^{2}})
#'
#' @description Calculates the variance-covariance matrix of estimates of regression coefficients using
#'   \deqn{
#'     \hat{\sigma}_{\varepsilon}^2 \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'   }
#'   where \eqn{\hat{\sigma}_{\varepsilon}^{2}}
#'   is the estimate of the error variance \eqn{\sigma_{\varepsilon}^{2}}
#'   and \eqn{\mathbf{X}} is the data matrix, that is,
#'   an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
#'   which includes a regressor whose value is 1 for each observation on the first column.
#'
#' @details If `sigma2epsilonhat = NULL`, `sigma2epsilonhat` is computed
#'   using [`sigma2epsilonhat()`].
#'
#' @family variance-covariance of regression coefficients functions
#' @keywords inference
#' @param sigma2epsilonhat Numeric.
#'   Estimate of error variance.
#' @inheritParams sigma2epsilonhat
#' @return Returns the variance-covariance matrix
#'   of estimates of regression coefficients.
#' @inherit sigma2epsilonhat references
#' @export
.vcovbetahat <- function(sigma2epsilonhat = NULL,
                         X,
                         y) {
  if (is.null(sigma2epsilonhat)) {
    sigma2epsilonhat <- sigma2epsilonhat(
      X = X,
      y = y
    )
  }
  unname(
    sigma2epsilonhat * solve(crossprod(X))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family variance-covariance of regression coefficients functions
#' @keywords inference
#' @param sigma2epsilonhatbiased Numeric.
#'   Biased estimate of error variance.
#' @inheritParams sigma2epsilonhatbiased
#' @inherit sigma2epsilonhatbiased references
#' @export
.vcovbetahatbiased <- function(sigma2epsilonhatbiased = NULL,
                               X,
                               y) {
  if (is.null(sigma2epsilonhatbiased)) {
    sigma2epsilonhatbiased <- sigma2epsilonhatbiased(
      X = X,
      y = y
    )
  }
  unname(
    sigma2epsilonhatbiased * solve(crossprod(X))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients
#'
#' @family variance-covariance of regression coefficients functions
#' @keywords inference
#' @inheritParams .vcovbetahat
#' @inherit .vcovbetahat return description references
#' @export
vcovbetahat <- function(X,
                        y) {
  .vcovbetahat(
    sigma2epsilonhat = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family variance-covariance of regression coefficients functions
#' @keywords inference
#' @inheritParams .vcovbetahatbiased
#' @inherit .vcovbetahatbiased return description references
#' @export
vcovbetahatbiased <- function(X,
                              y) {
  .vcovbetahatbiased(
    sigma2epsilonhatbiased = NULL,
    X = X,
    y = y
  )
}

#' Residual Variance (from \eqn{RSS})
#'
#' Calculates estimates of the error variance
#'   \deqn{
#'     \mathbf{E}
#'       \left(
#'         \sigma^2
#'       \right)
#'   =
#'   \hat{\sigma}^2
#'   }
#'   \deqn{
#'     \hat{\sigma}^2_{\textrm{unbiased}}
#'     =
#'     \frac{1}{n - k}
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X} \boldsymbol{\hat{\beta}}
#'     \right)^2 \\
#'     =
#'     \frac{
#'     \mathbf{e}^{\prime}
#'     \mathbf{e}
#'     }
#'     {
#'       n - k
#'     } \\
#'     =
#'     \frac{
#'     RSS
#'     }
#'     {
#'       n - k
#'     }
#'   }
#' or
#'   \deqn{
#'     \hat{\sigma}^2_{\textrm{biased}}
#'     =
#'     \frac{1}{n}
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X} \boldsymbol{\hat{\beta}}
#'     \right)^2 \\
#'     =
#'     \frac{
#'     \mathbf{e}^{\prime}
#'     \mathbf{e}
#'     }
#'     {
#'       n
#'     } \\
#'     =
#'     \frac{
#'     RSS
#'     }
#'     {
#'       n
#'     }
#'   }
#' where
#' \eqn{\mathbf{e}} is the vector of residuals,
#' \eqn{RSS} is the residual sum of squares,
#' \eqn{n} is the sample size,
#' and \eqn{k} is the number of regressors
#' including a regressor whose value is 1 for each observation.
#'
#' If `RSS = NULL`,
#' `RSS` is computed
#' using [`RSS()`]
#' with `X` and `y` as a required arguments
#' and `betahat` as an optional argument.
#' If `RSS` is provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams y_minus_yhat
#' @inheritParams .Rbar2
#' @inheritParams .R2_RSS
#' @param type String.
#'   Residual variance estimator.
#'   If `type = "unbiased"`,
#'   returns unbiased estimate.
#'   If `type = "biased"`,
#'   returns biased estimate.
#'   If `type = "both"`,
#'   returns a vector of
#'   unbiased and biased estimates.
#' @return Returns the estimated
#'   residual variance (\eqn{\hat{\sigma}^2}).
#' @inherit yhat references
#' @family residual variance functions
#' @export
.sigma2hat <- function(RSS = NULL,
                       n,
                       k,
                       type = "unbiased",
                       X = NULL,
                       y = NULL,
                       betahat = NULL) {
  if (is.null(RSS)) {
    RSS <- RSS(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (type == "unbiased") {
    return(RSS / (n - k))
  }
  if (type == "biased") {
    return(RSS / n)
  }
  if (type == "both") {
    return(
      c(
        unbiased = RSS / (n - k),
        biased = RSS / n
      )
    )
  }
}

#' Residual Variance (\eqn{\hat{\sigma}^2})
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .sigma2hat
#' @inherit .sigma2hat return description references
#' @family residual variance functions
#' @export
sigma2hat <- function(X,
                      y,
                      type = "unbiased") {
  .sigma2hat(
    RSS = NULL,
    type = type,
    betahat = NULL,
    X = X,
    y = y
  )
}

#' Variance-Covariance Matrix of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}^2})
#'
#' Calculates the variance-covariance matrix
#' of estimates of regression coefficients using
#'   \deqn{
#'     \hat{\sigma}^2
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{X}
#'     \right)^{-1}
#'   }
#' where \eqn{\hat{\sigma}^2}
#' is the estimate of the error variance \eqn{{\sigma}^2}
#' and \eqn{\mathbf{X}} is
#' the data matrix,
#' that is,
#' an \eqn{n \times k} matrix
#' of \eqn{n} observations
#' of \eqn{k} regressors,
#' which includes a regressor
#' whose value is 1 for each observation.
#'
#' If `sigma2hat = NULL`,
#' `sigma2hat` is computed
#' using [`sigma2hat()`]
#' with `X` and `y` as a required arguments
#' and `betahat` and `RSS` as an optional argument.
#' If `sigma2hat` is provided,
#' `RSS`, `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param sigma2hat Numeric.
#'   Estimate of error variance.
#' @inheritParams .sigma2hat
#' @return Returns the variance-covariance matrix
#'   of estimates of regression coefficients.
#' @inherit sigma2hat references
#' @family variance-covariance functions
#' @export
.vcov_betahat <- function(sigma2hat = NULL,
                          RSS = NULL,
                          X,
                          y,
                          betahat = NULL,
                          type = "unbiased") {
  if (is.null(sigma2hat)) {
    sigma2hat <- .sigma2hat(
      RSS = RSS,
      type = type,
      betahat = betahat,
      X = X,
      y = y
    )
  }
  inv <- solve(crossprod(X))
  if (type == "both") {
    return(
      list(
        unbiased = sigma2hat["unbiased"] * inv,
        biased = sigma2hat["biased"] * inv
      )
    )
  } else if (type == "unbiased" | type == "biased") {
    return(
      unname(
        sigma2hat * inv
      )
    )
  }
}

#' Variance-Covariance Matrix of Estimates of Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .vcov_betahat
#' @inherit .vcov_betahat return description references
#' @family variance-covariance functions
#' @export
vcov_betahat <- function(X,
                         y,
                         type = "unbiased") {
  .vcov_betahat(
    sigma2hat = NULL,
    betahat = NULL,
    X = X,
    y = y,
    type = type
  )
}

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
#'     \frac{
#'     \mathbf{e}^{\prime}
#'     \mathbf{e}
#'     }
#'     {
#'       n - k
#'     }
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
#'     \frac{
#'     \mathbf{e}^{\prime}
#'     \mathbf{e}
#'     }
#'     {
#'       n
#'     }
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
#' If `rss = NULL`,
#' `rss` is computed
#' using [`ss_r()`]
#' with `X` and `y` as a required arguments
#' and `beta_hat` as an optional argument.
#' If `rss` is provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams e_y_minus_y_hat
#' @inheritParams rbar2_r2
#' @inheritParams r2_rss
#' @param type String.
#'   Residual variance estimator.
#'   If `type = "unbiased"`,
#'   returns unbiased estimate.
#'   If `type = "biased"`,
#'   returns biased estimate.
#'   If `type = "both"`,
#'   returns a vector of
#'   unbiased and biased estimates.
#' @return Returns the estimated residual variance.
#' @inherit y_hat references
#' @export
sigma2_hat_rss <- function(rss = NULL,
                           n,
                           k,
                           type = "unbiased",
                           beta_hat = NULL,
                           X = NULL,
                           y = NULL) {
  if (is.null(rss)) {
    rss <- ss_r(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (type == "unbiased") {
    return(rss / (n - k))
  }
  if (type == "biased") {
    return(rss / n)
  }
  if (type == "both") {
    return(
      c(
        unbiased = rss / (n - k),
        biased = rss / n
      )
    )
  }
}

#' Residual Variance
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams sigma2_hat_rss
#' @inherit sigma2_hat_rss return description references
#' @export
sigma2_hat <- function(X,
                       y,
                       type = "unbiased") {
  sigma2_hat_rss(
    rss = NULL,
    type = type,
    beta_hat = NULL,
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
#' If `sigma2_hat = NULL`,
#' `sigma2_hat` is computed
#' using [`sigma2_hat()`]
#' with `X` and `y` as a required arguments
#' and `beta_hat` as an optional argument.
#' If `sigma2_hat` is provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param sigma2_hat Numeric.
#'   Estimate of error variance.
#' @inheritParams sigma2_hat_rss
#' @return Returns variance-covariance matrix of estimates of regression coefficients.
#' @inherit sigma2_hat references
#' @export
cov_beta_hat_sigma2_hat <- function(sigma2_hat = NULL,
                                    beta_hat = NULL,
                                    X,
                                    y,
                                    type = "unbiased") {
  if (is.null(sigma2_hat)) {
    sigma2_hat <- sigma2_hat(
      beta_hat = beta_hat,
      X = X,
      y = y,
      type = type
    )
  }
  inv <- solve(crossprod(X))
  if (type == "both") {
    return(
      list(
        unbiased = sigma2_hat["unbiased"] * inv,
        biased = sigma2_hat["biased"] * inv
      )
    )
  } else if (type == "unbiased" | type == "biased") {
    return(
      unname(
        sigma2_hat * inv
      )
    )
  }
}

#' Variance-Covariance Matrix of Estimates of Regression Coefficients
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams cov_beta_hat_sigma2_hat
#' @inherit cov_beta_hat_sigma2_hat return description references
#' @export
cov_beta_hat <- function(X,
                         y,
                         type = "unbiased") {
  cov_beta_hat_sigma2_hat(
    sigma2_hat = NULL,
    beta_hat = NULL,
    X = X,
    y = y,
    type = type
  )
}

#' Residual Variance
#'
#' Calculates estimates of the residual variance
#'   \deqn{
#'     \mathbf{E}
#'       \left(
#'         \sigma^2
#'       \right)
#'   =
#'   s^2
#'   },
#'   \deqn{
#'     s_{\textrm{OLS}}^{2}
#'     =
#'     \frac{
#'       \mathbf{e^{\prime} e }
#'     }
#'     {
#'       n - k
#'     }
#'   },
#'   \deqn{
#'     s_{\textrm{ML}}^{2}
#'     =
#'     \frac{
#'       \mathbf{e^{\prime} e }
#'     }
#'     {
#'       n
#'     }
#'   }.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams e_y_minus_y_hat
#' @param s_sqr_est String.
#'   Residual variance estimator.
#'   If \code{"both"},
#'   returns both OLS and ML estimates as a vector.
#'   If \code{"ols"},
#'   returns OLS estimate.
#'   If \code{"ml"},
#'   returns ML estimate.
#' @return Returns the estimated residual variance.
#' @export
s_sqr <- function(beta_hat = NULL,
                  X,
                  y,
                  s_sqr_est = "both") {
  rss <- ss_r(
    beta_hat = beta_hat,
    X = X,
    y = y
  )
  ols <- rss / (nrow(X) - ncol(X))
  ml <- rss / nrow(X)
  out <- c(
    ols = ols,
    ml = ml
  )
  if (s_sqr_est == "ols") {
    return(
      ols
    )
  }
  if (s_sqr_est == "ml") {
    return(
      ml
    )
  }
  if (s_sqr_est == "both") {
    return(
      out
    )
  }
  out
}

#' Variance-Covariance Matrix of Estimates Regression Coefficients
#'
#' Calculates the variance-covariance matrix of estimates of regression coefficients
#'   \deqn{
#'     \sigma^2 \left(\mathbf{X}^{\prime} \mathbf{X} \right)^{-1}.
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams s_sqr
#' @return Returns variance-covariance matrix of estimates of regression coefficients.
#' @export
cov_beta_hat <- function(beta_hat = NULL,
                         X,
                         y,
                         s_sqr_est = "ols") {
  s_sqr <- s_sqr(
    beta_hat = beta_hat,
    X = X,
    y = y,
    s_sqr_est = s_sqr_est
  )
  mat <- solve(crossprod(X))
  if (s_sqr_est == "both") {
    return(
      list(
        ols = s_sqr["ols"] * mat,
        ml = s_sqr["ml"] * mat
      )
    )
  } else if (s_sqr_est == "ols" | s_sqr_est == "ml") {
    return(
      unname(
        s_sqr * mat
      )
    )
  }
}

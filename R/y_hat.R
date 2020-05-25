#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{P} \mathbf{y}})
#'
#' Calculates y-hat using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{P}
#'     \mathbf{y}.
#'   }
#'
#' If `P = NULL`,
#' the `P` matrix is computed
#' using [`proj_P()`]
#' with `X` as its argument.
#' If `P` is provided,
#' `X` is not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams beta_hat_inv
#' @inheritParams proj_M
#' @return
#'   Returns y-hat (\eqn{\mathbf{\hat{y}}}).
#' @family y-hat functions
#' @export
y_hat_Py <- function(y,
                     P = NULL,
                     X = NULL) {
  if (is.null(P)) {
    if (is.null(X)) {
      stop(
        "If `P` is NULL, `X` should be provided."
      )
    }
    P <- proj_P(X = X)
  }
  P %*% y
}

#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `beta_hat = NULL`,
#' the `beta_hat` vector is computed
#' using [`beta_hat_inv()`]
#' with `X` and `y`
#' as arguments.
#' If `beta_hat` is provided,
#' `y` is not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams beta_hat_inv
#' @inheritParams e_y_minus_y_hat
#' @inherit y_hat_Py return
#' @family y-hat functions
#' @export
y_hat_Xbeta_hat <- function(X,
                            beta_hat = NULL,
                            y = NULL) {
  if (is.null(beta_hat)) {
    beta_hat <- beta_hat_inv(
      X = X,
      y = y
    )
  }
  X %*% beta_hat
}

#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `beta_hat = NULL`,
#' the `beta_hat` vector is computed
#' using [`beta_hat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams y_hat_Xbeta_hat
#' @inherit y_hat_Py return
#' @family y-hat functions
#' @export
y_hat <- function(X,
                  y,
                  beta_hat = NULL) {
  y_hat_Xbeta_hat(
    X = X,
    beta_hat = beta_hat,
    y = y
  )
}

#' y-hat for Testing Data  Set (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}
#'   }
#' for the **testing data set**.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param beta_hat Vector or `k` by `1` matrix.
#'   \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients
#'   (\eqn{\boldsymbol{\hat{\beta}}}).
#'   Estimated from **training data set**.
#' @param X Matrix.
#'   The data matrix (\eqn{\mathbf{X}}),
#'   that is an \eqn{n \times k} matrix of \eqn{n} observations
#'   of \eqn{k} regressors,
#'   which includes a regressor whose value is 1 for each observation
#'   from the **testing data set**.
#' @param y Vector or `n` by `1` matrix.
#'   \eqn{n \times 1} vector of observations on the regressand variable
#'   (\eqn{\mathbf{y}})
#'   from the **testing data set**.
#'   This is an optional argument
#'   for model performance assessment.
#' @return
#'   Returns a list with the following elements
#'   \describe{
#'     \item{y_hat}{Predicted values of \eqn{\mathbf{y}} using the `beta_hat` (\eqn{\mathbf{\hat{\beta}}}) from traning data set and `X` (\eqn{\mathbf{X}}) from testing data set.}
#'     \item{RSS}{Residual sum of squares. `NA` if `y = NULL`.}
#'     \item{MSE}{Mean-square error. `NA` if `y = NULL`.}
#'     \item{RMSE}{Root-mean-square error. `NA` if `y = NULL`.}
#'     \item{R_squared}{Coefficient of determinism. `NA` if `y = NULL`.}
#'     \item{adjusted_R_squared}{Adjusted R-squared. `NA` if `y = NULL`.}
#'   }
#' @family y-hat functions
#' @export
y_hat_test <- function(beta_hat,
                       X,
                       y = NULL) {
  y_hat <- y_hat_Xbeta_hat(
    X = X,
    beta_hat = beta_hat,
    y = NULL
  )
  if (is.null(y)) {
    mse <- NA
    rmse <- NA
  } else {
    rss <- sum((y - y_hat)^2)
    tss <- ss_t(y = y)
    mse <- mean(rss)
    rmse <- sqrt(mse)
    r2 <- 1 - (rss / tss)
    r_bar_sqr <- r_bar_sqr(
      r2 = r2,
      n = nrow(X),
      k = ncol(X)
    )
  }
  list(
    y_hat = y_hat,
    RSS = rss,
    MSE = mse,
    RMSE = rmse,
    R_squared = r2,
    adjusted_R_squared = r_bar_sqr
  )
}

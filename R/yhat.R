#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{P} \mathbf{y}})
#'
#' Calculates y-hat (\eqn{\mathbf{\hat{y}}}), that is,
#' the predicted value of \eqn{\mathbf{y}}
#' given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{P}
#'     \mathbf{y}.
#'   }
#'
#' If `P = NULL`,
#' the `P` matrix is computed
#' using [`P()`]
#' with `X` as its argument.
#' If `P` is provided,
#' `X` is not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams betahat_inv
#' @inheritParams M
#' @return
#'   Returns y-hat (\eqn{\mathbf{\hat{y}}}).
#' @family y-hat functions
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#' @export
Py <- function(y,
               P = NULL,
               X = NULL) {
  if (is.null(P)) {
    if (is.null(X)) {
      stop(
        "If `P` is NULL, `X` should be provided."
      )
    }
    P <- P(X = X)
  }
  P %*% y
}

#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat (\eqn{\mathbf{\hat{y}}}), that is,
#' the predicted value of \eqn{\mathbf{y}}
#' given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `betahat = NULL`,
#' the `betahat` vector is computed
#' using [`betahat_inv()`]
#' with `X` and `y`
#' as arguments.
#' If `betahat` is provided,
#' `y` is not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams betahat_inv
#' @inheritParams y_minus_yhat
#' @inherit Py return references
#' @family y-hat functions
#' @export
Xbetahat <- function(X,
                     betahat = NULL,
                     y = NULL) {
  if (is.null(betahat)) {
    betahat <- betahat_inv(
      X = X,
      y = y
    )
  }
  X %*% betahat
}

#' y-hat (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat (\eqn{\mathbf{\hat{y}}}), that is,
#' the predicted value of \eqn{\mathbf{y}}
#' given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `betahat = NULL`,
#' the `betahat` vector is computed
#' using [`betahat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams Xbetahat
#' @inherit Py return references
#' @family y-hat functions
#' @export
yhat <- function(X,
                 y,
                 betahat = NULL) {
  Xbetahat(
    X = X,
    betahat = betahat,
    y = y
  )
}

#' y-hat for Testing Data  Set (\eqn{\mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}})
#'
#' Calculates y-hat (\eqn{\mathbf{\hat{y}}}), that is,
#' the predicted value of \eqn{\mathbf{y}}
#' given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}
#'   }
#' for the **testing data set**.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param betahat Vector or `k` by `1` matrix.
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
#'     \item{yhat}{Predicted values of \eqn{\mathbf{y}} using the `betahat` (\eqn{\mathbf{\hat{\beta}}}) from traning data set and `X` (\eqn{\mathbf{X}}) from testing data set.}
#'     \item{RSS}{Residual sum of squares. `NA` if `y = NULL`.}
#'     \item{MSE}{Mean-square error. `NA` if `y = NULL`.}
#'     \item{RMSE}{Root-mean-square error. `NA` if `y = NULL`.}
#'     \item{R_squared}{Coefficient of determinism. `NA` if `y = NULL`.}
#'     \item{adjusted_R_squared}{Adjusted R-squared. `NA` if `y = NULL`.}
#'   }
#' @inherit Py references
#' @family y-hat functions
#' @export
yhat_test <- function(betahat,
                      X,
                      y = NULL) {
  yhat <- Xbetahat(
    X = X,
    betahat = betahat,
    y = NULL
  )
  if (is.null(y)) {
    rss <- NA
    tss <- NA
    mse <- NA
    rmse <- NA
    r2 <- NA
    rbar2 <- NA
  } else {
    rss <- sum((y - yhat)^2)
    tss <- tss(y = y)
    mse <- mean(rss)
    rmse <- sqrt(mse)
    r2 <- 1 - (rss / tss)
    rbar2 <- .rbar2(
      r2 = r2,
      n = nrow(X),
      k = ncol(X)
    )
  }
  list(
    yhat = yhat,
    RSS = rss,
    MSE = mse,
    RMSE = rmse,
    R_squared = r2,
    adjusted_R_squared = rbar2
  )
}

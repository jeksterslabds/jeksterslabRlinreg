#' y-hat
#' \eqn{
#'   \left(
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{P}
#'     \mathbf{y}
#'   \right)
#' }
#'
#' Calculates y-hat
#' \eqn{
#'   \left(
#'     \mathbf{\hat{y}}
#'   \right)
#' },
#' that is,
#' the predicted value of
#' \eqn{
#'   \mathbf{y}
#' }
#' given
#' \eqn{
#'   \mathbf{X}
#' }
#' using
#' \deqn{
#'   \mathbf{\hat{y}}
#'   =
#'   \mathbf{P}
#'   \mathbf{y}.
#' }
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
#' Returns y-hat
#' \eqn{\left( \mathbf{\hat{y}} \right)}.
#' @family y-hat functions
#' @references
#' [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#' [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
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

#' y-hat
#' \eqn{\left( \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'
#' Calculates y-hat
#' \eqn{\left( \mathbf{\hat{y}} \right)},
#' that is,
#' the predicted value of
#' \eqn{\mathbf{y}}
#' given
#' \eqn{\mathbf{X}} using
#' \deqn{
#'   \mathbf{\hat{y}}
#'   =
#'   \mathbf{X}
#'   \boldsymbol{\hat{\beta}}.
#' }
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

#' y-hat
#' \eqn{\left( \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'
#' Calculates y-hat
#' \eqn{\left( \mathbf{\hat{y}} \right)},
#' that is,
#' the predicted value of
#' \eqn{\mathbf{y}}
#' given
#' \eqn{\mathbf{X}} using
#' \deqn{
#'   \mathbf{\hat{y}}
#'   =
#'   \mathbf{X}
#'   \boldsymbol{\hat{\beta}}.
#' }
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

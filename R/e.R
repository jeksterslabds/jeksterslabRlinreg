#' Residuals (\eqn{\mathbf{e} = \mathbf{My}})
#'
#' Calculates residuals using
#'   \deqn{
#'     \mathbf{e}
#'     =
#'     \mathbf{My}.
#'   }
#'
#' If `M = NULL`,
#' the `M` matrix is computed
#' using [`M()`]
#' with `X` as a required argument
#' and `P` as an optional argument.
#' If `M` is provided,
#' `X` and `P` are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param M Matrix.
#'   The residual maker matrix (\eqn{\mathbf{M}}).
#' @inheritParams betahat_inv
#' @inheritParams M
#' @return
#'   Returns residuals (\eqn{\mathbf{e}}),
#'   that is,
#'   the difference between
#'   the observed and predicted value of \eqn{\mathbf{y}}
#'   (\eqn{\mathbf{e} = \mathbf{y}} - \eqn{\mathbf{\hat{y}}}).
#' @references
#'   [Wikipedia: Errors and Residuals](https://en.wikipedia.org/wiki/Errors_and_residuals)
#' @family residuals functions
#' @export
My <- function(y,
               M = NULL,
               X = NULL,
               P = NULL) {
  if (is.null(M)) {
    M <- M(
      X = X,
      P = P
    )
  }
  M %*% y
}

#' Residuals (\eqn{\mathbf{e} = \mathbf{y} - \mathbf{\hat{y}}})
#'
#' Calculates residuals using
#'   \deqn{
#'     \mathbf{e}
#'     =
#'     \mathbf{y}
#'     -
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{y}
#'     -
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `yhat = NULL`,
#' the `yhat` vector is computed
#' using [`Xbetahat()`]
#' with `X` as a required argument
#' and `betahat` as an optional argument.
#' If `yhat` is provided,
#' `X` and `betahat` are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param yhat Vector or `n` by `1` matrix.
#'   \eqn{n \times 1} vector
#'   of predicted values of \eqn{\mathbf{y}}
#'   (\eqn{\mathbf{\hat{y}}}).
#' @param betahat Vector or `k` by `1` matrix.
#'   The vector
#'   \eqn{\boldsymbol{\hat{\beta}}}
#'   is a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients.
#' @inheritParams betahat_inv
#' @inherit My return references
#' @family residuals functions
#' @export
y_minus_yhat <- function(y,
                         yhat = NULL,
                         X = NULL,
                         betahat = NULL) {
  if (is.null(yhat)) {
    if (is.null(X)) {
      stop(
        "If `yhat` is NULL, `X` should be provided."
      )
    }
    yhat <- Xbetahat(
      X = X,
      betahat = betahat,
      y = y
    )
  }
  y - yhat
}

#' Residuals (\eqn{\mathbf{e} = \mathbf{y} - \mathbf{\hat{y}}})
#'
#' Calculates residuals using
#'   \deqn{
#'     \mathbf{e}
#'     =
#'     \mathbf{y}
#'     -
#'     \mathbf{\hat{y}}
#'     =
#'     \mathbf{y}
#'     -
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'
#' If `betahat = NULL`,
#' the `betahat` vector is computed
#' using [`betahat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams y_minus_yhat
#' @inherit My return references
#' @family residuals functions
#' @export
e <- function(X,
              y,
              betahat = NULL) {
  y_minus_yhat(
    y = y,
    yhat = NULL,
    X = X,
    betahat = betahat
  )
}

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
#' using [`proj_M()`]
#' with `X` as a required argument
#' and `P` as an optional argument.
#' If `M` is provided,
#' `X` and `P` are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param M Matrix.
#'   The residual maker matrix (\eqn{\mathbf{M}}).
#' @inheritParams beta_hat_inv
#' @inheritParams proj_M
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
e_My <- function(y,
                 M = NULL,
                 X = NULL,
                 P = NULL) {
  if (is.null(M)) {
    M <- proj_M(
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
#' If `y_hat = NULL`,
#' the `y_hat` vector is computed
#' using [`y_hat_Xbeta_hat()`]
#' with `X` as a required argument
#' and `beta_hat` as an optional argument.
#' If `y_hat` is provided,
#' `X` and `beta_hat` are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param y_hat Vector or `n` by `1` matrix.
#'   \eqn{n \times 1} vector
#'   of predicted values of \eqn{\mathbf{y}}
#'   (\eqn{\mathbf{\hat{y}}}).
#' @param beta_hat Vector or `k` by `1` matrix.
#'   The vector
#'   \eqn{\boldsymbol{\hat{\beta}}}
#'   is a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients.
#' @inheritParams beta_hat_inv
#' @inherit e_My return references
#' @family residuals functions
#' @export
e_y_minus_y_hat <- function(y,
                            y_hat = NULL,
                            X = NULL,
                            beta_hat = NULL) {
  if (is.null(y_hat)) {
    if (is.null(X)) {
      stop(
        "If `y_hat` is NULL, `X` should be provided."
      )
    }
    y_hat <- y_hat_Xbeta_hat(
      X = X,
      beta_hat = beta_hat,
      y = y
    )
  }
  y - y_hat
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
#' If `beta_hat = NULL`,
#' the `beta_hat` vector is computed
#' using [`beta_hat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams e_y_minus_y_hat
#' @inherit e_My return references
#' @family residuals functions
#' @export
e <- function(X,
              y,
              beta_hat = NULL) {
  e_y_minus_y_hat(
    y = y,
    y_hat = NULL,
    X = X,
    beta_hat = beta_hat
  )
}

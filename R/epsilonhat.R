#' Residuals (\eqn{\boldsymbol{\hat{\varepsilon}} = \mathbf{My}})
#'
#' Calculates residuals using
#'   \deqn{
#'     \boldsymbol{\hat{\varepsilon}}
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
#'   The residual maker matrix
#'   (\eqn{\mathbf{M}}).
#' @inheritParams betahat_inv
#' @inheritParams M
#' @return
#'   Returns
#'   an
#'   \eqn{n \times 1}
#'   matrix of
#'   residuals
#'   (\eqn{\boldsymbol{\hat{\varepsilon}}}),
#'   that is,
#'   the difference between
#'   the observed
#'   (\eqn{\mathbf{y}})
#'   and predicted
#'   (\eqn{\mathbf{\hat{y}}})
#'   values of
#'   the regressand variable
#'   (\eqn{\boldsymbol{\hat{\varepsilon}} = \mathbf{y}} - \eqn{\mathbf{\hat{y}}}).
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

#' Residuals (\eqn{\boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}}})
#'
#' Calculates residuals using
#'   \deqn{
#'     \hat{\varepsilon}_{i}
#'     =
#'     Y_{i}
#'     -
#'     \hat{Y}_{i} \\
#'     =
#'     Y_{i}
#'     -
#'     \left(
#'       \hat{\beta}_{1}
#'       +
#'       \hat{\beta}_{2}
#'       X_{2i}
#'       +
#'       \hat{\beta}_{3}
#'       X_{3i}
#'       +
#'       \dots
#'       +
#'       \hat{\beta}_{k}
#'       X_{ki}
#'     \right) \\
#'     =
#'     Y_{i}
#'     -
#'     \hat{\beta}_{1}
#'     -
#'     \hat{\beta}_{2}
#'     X_{2i}
#'     -
#'     \hat{\beta}_{3}
#'     X_{3i}
#'     -
#'     \dots
#'     -
#'     \hat{\beta}_{k}
#'     X_{ki} .
#'   }
#' In matrix form
#'   \deqn{
#'     \boldsymbol{\hat{\varepsilon}}
#'     =
#'     \mathbf{y}
#'     -
#'     \mathbf{\hat{y}} \\
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

#' Residuals (\eqn{\boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}}})
#'
#' @details If `betahat = NULL`,
#' the `betahat` vector is computed
#' using [`betahat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams y_minus_yhat
#' @inherit y_minus_yhat description return references
#' @family residuals functions
#' @export
epsilonhat <- function(X,
                       y,
                       betahat = NULL) {
  y_minus_yhat(
    y = y,
    yhat = NULL,
    X = X,
    betahat = betahat
  )
}

##############################################
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
##############################################



.zepsilonhat <- function(epsilonhat = NULL,
                         X,
                         y,
                         betahat = NULL) {
  if (is.null(epsilonhat)) {
    epsilonhat <- epsilonhat(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  scale(
    x = epsilonhat,
    center = TRUE,
    scale = TRUE
  )
}

zepsilonhat <- function(X,
                        y,
                        betahat = NULL) {
  .zepsilonhat(
    epsilonhat = NULL,
    X = X,
    y = y,
    betahat = NULL
  )
}

.tepsilonhat <- function(epsilonhat = NULL,
                         h = NULL,
                         sigma2hat = NULL,
                         X = NULL,
                         y = NULL,
                         betahat = NULL) {
  if (is.null(epsilonhat)) {
    epsilonhat <- epsilonhat(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  if (is.null(h)) {
    h <- h(X)
  }
  if (is.null(sigma2hat)) {
    sigma2hat <- sigma2hat(
      X = X,
      y = y,
      type = "unbiased"
    )
  }
  epsilonhat / sqrt(sigma2hat * (1 - h))
}


tepsilonhat <- function(X,
                        y,
                        betahat = NULL) {
  .tepsilonhat(
    epsilonhat = NULL,
    h = NULL,
    sigma2hat = NULL,
    X = X,
    y = y,
    betahat = betahat
  )
}

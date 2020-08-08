#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residuals
#'   \eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{My} \right)}
#'
#' @description Calculates residuals using
#'   \deqn{
#'     \boldsymbol{\hat{\varepsilon}} =  \mathbf{My} .
#'   }
#'   where
#'   \deqn{
#'     \mathbf{M}
#'     = \mathbf{I} - \mathbf{P} \\
#'     = \mathbf{I} - \mathbf{X}
#'       \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1} \mathbf{X}^{T} .
#'   }
#'
#' @details If `M = NULL`, the `M` matrix is computed using [`M()`]
#'   with `X` as a required argument and `P` as an optional argument.
#'   If `M` is provided, `X` and `P` are not needed.
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams betahat
#' @inheritParams .M
#' @param M `n` by `n` numeric matrix.
#'   The \eqn{n \times n} residual maker matrix \eqn{\left( \mathbf{M} \right)}.
#' @return Returns an \eqn{n \times 1} matrix of residuals
#'   \eqn{\left( \boldsymbol{\hat{\varepsilon}} \right)},
#'   that is, the difference between the observed \eqn{\left( \mathbf{y} \right)}
#'   and predicted \eqn{\left( \mathbf{\hat{y}} \right)} values of the regressand variable
#'   \eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}} \right)}.
#' @references
#'   [Wikipedia: Errors and Residuals](https://en.wikipedia.org/wiki/Errors_and_residuals)
#' @export
.My <- function(y,
                M = NULL,
                X = NULL,
                P = NULL) {
  if (is.null(M)) {
    if (is.null(X)) {
      stop(
        "`X` is needed when `M` is not provided."
      )
    }
    M <- .M(
      X = X,
      P = P
    )
  }
  M %*% y
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residuals
#'   \eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{My} \right)}
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams .My
#' @inherit .My description return references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' My(X = X, y = y)
#' @export
My <- function(X,
               y) {
  .My(
    y = y,
    M = NULL,
    X = X,
    P = NULL
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residuals
#'   \eqn{
#'     \left( \boldsymbol{\hat{\varepsilon}}
#'     = \mathbf{y} - \mathbf{\hat{y}} \right)
#'   }
#'
#' @description Calculates residuals using
#'   \deqn{
#'     \hat{\varepsilon}_{i}
#'     = Y_{i} - \hat{Y}_{i} \\
#'     = Y_{i} - \left( \hat{\beta}_{1} + \hat{\beta}_{2} X_{2i} +
#'       \hat{\beta}_{3} X_{3i} + \dots + \hat{\beta}_{k} X_{ki} \right) \\
#'     = Y_{i} - \hat{\beta}_{1} - \hat{\beta}_{2} X_{2i} -
#'       \hat{\beta}_{3} X_{3i} - \dots - \hat{\beta}_{k} X_{ki} .
#'   }
#'   In matrix form
#'   \deqn{
#'     \boldsymbol{\hat{\varepsilon}}
#'     = \mathbf{y} - \mathbf{\hat{y}} \\
#'     = \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} .
#'   }
#'
#' @details If `yhat = NULL`, the `yhat` vector is computed using [`Xbetahat()`]
#'   with `X` as a required argument and `betahat` as an optional argument.
#'   If `yhat` is provided, `X` and `betahat` are not needed.
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams betahat
#' @inheritParams .Xbetahat
#' @inherit My return references
#' @param yhat Numeric vector of length `n` or `n` by `1` numeric matrix.
#'   \eqn{n \times 1} vector of predicted values of \eqn{\mathbf{y}}
#'   \eqn{\left( \mathbf{\hat{y}} \right)}.
#' @export
.yminusyhat <- function(y,
                        yhat = NULL,
                        X = NULL,
                        betahat = NULL) {
  if (is.null(yhat)) {
    if (is.null(X)) {
      stop(
        "If `yhat` is NULL, `X` should be provided."
      )
    }
    yhat <- .Xbetahat(
      X = X,
      betahat = betahat,
      y = y
    )
  }
  y - yhat
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residuals
#'   \eqn{
#'     \left( \boldsymbol{\hat{\varepsilon}}
#'     = \mathbf{y} - \mathbf{\hat{y}} \right)
#'   }
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams .yminusyhat
#' @inherit .yminusyhat description return references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' epsilonhat <- yminusyhat(X = X, y = y)
#' hist(epsilonhat)
#' @export
yminusyhat <- function(X,
                       y) {
  .yminusyhat(
    y = y,
    yhat = NULL,
    X = X,
    betahat = NULL
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residuals
#'   \eqn{
#'     \left( \boldsymbol{\hat{\varepsilon}}
#'     = \mathbf{y} - \mathbf{\hat{y}} \right)
#'   }
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams yminusyhat
#' @inherit yminusyhat description return references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' epsilonhat <- epsilonhat(X = X, y = y)
#' hist(epsilonhat)
#' @export
epsilonhat <- function(X,
                       y) {
  yminusyhat(
    X = X,
    y = y
  )
}

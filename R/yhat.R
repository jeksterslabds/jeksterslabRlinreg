#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title y-hat \eqn{\left( \mathbf{\hat{y}} = \mathbf{P} \mathbf{y} \right)}
#'
#' @description Calculates y-hat \eqn{\left( \mathbf{\hat{y}} \right)},
#'   that is, the predicted value of \eqn{\mathbf{y}} given \eqn{\mathbf{X}}
#'   using
#'   \deqn{
#'     \mathbf{\hat{y}} = \mathbf{P} \mathbf{y}
#'   }
#'   where
#'   \deqn{
#'     \mathbf{P} = \mathbf{X} \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \mathbf{X}^{T} .
#'   }
#'
#' @details If `P = NULL`, the `P` matrix is computed using [`P()`]
#'   with `X` as its argument. If `P` is provided, `X` is not needed.
#'
#' @family y-hat functions
#' @keywords predicted
#' @inheritParams betahat
#' @inheritParams .M
#' @return Returns y-hat \eqn{\left( \mathbf{\hat{y}} \right)}.
#' @references
#' [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#' [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#' @export
.Py <- function(y,
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title y-hat \eqn{\left( \mathbf{\hat{y}} = \mathbf{P} \mathbf{y} \right)}
#'
#' @family y-hat functions
#' @keywords predicted
#' @inheritParams .Py
#' @inherit .Py description return references
#' @export
Py <- function(X,
               y) {
  .Py(
    y = y,
    P = NULL,
    X = X
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title y-hat
#'   \eqn{\left( \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'
#' @description Calculates y-hat \eqn{\left( \mathbf{\hat{y}} \right)},
#'   that is, the predicted value of \eqn{\mathbf{y}} given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}
#'   }
#'   where
#'   \deqn{
#'     \boldsymbol{\hat{\beta}} = \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \left( \mathbf{X}^{T} \mathbf{y} \right) .
#'   }
#'
#' @details If `betahat = NULL`, the `betahat` vector is computed
#'   using [`betahat()`] with `X` and `y` as arguments.
#'   If `betahat` is provided, `y` is not needed.
#'
#' @family y-hat functions
#' @keywords predicted
#' @inheritParams betahat
#' @param betahat Numeric vector of length `k` or `k` by `1` matrix.
#'   The vector \eqn{\boldsymbol{\hat{\beta}}} is a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients.
#' @inherit Py return references
#' @export
.Xbetahat <- function(X,
                      betahat = NULL,
                      y = NULL) {
  if (is.null(betahat)) {
    if (is.null(y)) {
      stop(
        "If `betahat` is NULL, `y` should be provided."
      )
    }
    betahat <- betahat(
      X = X,
      y = y
    )
  }
  X %*% betahat
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title y-hat
#'   \eqn{\left( \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'
#' @family y-hat functions
#' @keywords predicted
#' @inheritParams .Xbetahat
#' @inherit .Xbetahat description return references
#' @export
Xbetahat <- function(X,
                     y) {
  .Xbetahat(
    X = X,
    betahat = NULL,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title y-hat
#'   \eqn{\left( \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'
#' @description Calculates y-hat \eqn{\left( \mathbf{\hat{y}} \right)},
#'   that is, the predicted value of \eqn{\mathbf{y}} given \eqn{\mathbf{X}} using
#'   \deqn{
#'     \mathbf{\hat{y}} = \mathbf{X} \boldsymbol{\hat{\beta}}.
#'   }
#'
#' @family y-hat functions
#' @keywords predicted
#' @inheritParams Xbetahat
#' @inherit Py return references
#' @export
yhat <- function(X,
                 y) {
  Xbetahat(
    X = X,
    y = y
  )
}

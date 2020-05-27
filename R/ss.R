#' Residual Sum of Square (from \eqn{\mathbf{e}})
#'
#' Calculates the residual sum of squares (RSS) using
#'   \deqn{
#'     RSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X}
#'       \boldsymbol{\hat{\beta}}
#'     \right)^{2}
#'     =
#'     \sum_{i = 1}^{n} e_{i}^{2}
#'     =
#'     \mathbf{e}^{\prime}
#'     \mathbf{e}.
#'   }
#'   Equivalent formulas
#'   \deqn{
#'     RSS
#'     =
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X}
#'       \boldsymbol{\hat{\beta}}
#'     \right)^{\prime}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X}
#'       \boldsymbol{\hat{\beta}}
#'     \right)
#'     =
#'     \mathbf{y}^{\prime}
#'     \mathbf{y}
#'     -
#'     2
#'     \boldsymbol{\hat{\beta}}
#'     \mathbf{X}^{\prime}
#'     \mathbf{y}
#'     +
#'     \boldsymbol{\hat{\beta}}^{\prime}
#'     \mathbf{X}^{\prime}
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#' Note that
#'   \deqn{
#'     TSS
#'     =
#'     ESS
#'     +
#'     RSS.
#'   }
#'
#' If `e = NULL`,
#' the `e` vector is computed
#' using [`e()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `e` is provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param e Numeric vector.
#'   Residuals.
#' @inheritParams y_minus_yhat
#' @references
#'   [Wikipedia: Residual Sum of Squares](https://en.wikipedia.org/wiki/Residual_sum_of_squares)
#'
#'   [Wikipedia: Explained Sum of Squares](https://en.wikipedia.org/wiki/Explained_sum_of_squares)
#'
#'   [Wikipedia: Total Sum of Squares](https://en.wikipedia.org/wiki/Total_sum_of_squares)
#'
#'   [Wikipedia: Coefficient of Determination](https://en.wikipedia.org/wiki/Coefficient_of_determination)
#' @family sum of squares functions
#' @return Returns residual sum of squares.
#' @export
.rss <- function(e = NULL,
                 betahat = NULL,
                 X,
                 y) {
  if (is.null(e)) {
    e <- e(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  drop(
    crossprod(e)
  )
}

#' Residual Sum of Square
#'
#' @details If `betahat = NULL`,
#'   the `betahat` vector is computed
#'   using [`betahat_inv()`].
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .rss
#' @inherit .rss description references return
#' @family sum of squares functions
#' @export
rss <- function(betahat = NULL,
                X,
                y) {
  .rss(
    e = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

#' Explained Sum of Squares
#'
#' Calculates the explained sum of squares (ESS)
#'   \deqn{
#'     ESS
#'     =
#'     \boldsymbol{\hat{\beta}}^{\prime}
#'     \mathbf{X}^{\prime}
#'     \mathbf{X}
#'     \boldsymbol{\hat{\beta}}
#'     -
#'     n
#'     \mathbf{\bar{Y}}^{2}.
#'   }
#' Note that
#'   \deqn{
#'     TSS
#'     =
#'     ESS
#'     +
#'     RSS.
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams rss
#' @return Returns the explained sum of squares.
#' @inherit rss references
#' @family sum of squares functions
#' @export
ess <- function(betahat = NULL,
                X,
                y) {
  if (is.null(betahat)) {
    betahat <- betahat_inv(
      X = X,
      y = y
    )
  }
  drop(
    (t(betahat) %*% t(X) %*% X %*% betahat) - (nrow(X) * mean(y)^2)
  )
}

#' Total Sum of Squares.
#'
#' Calculates the total sum of squares (TSS)
#'   \deqn{
#'     TSS
#'     =
#'     \mathbf{y}^{\prime}
#'     \mathbf{y}
#'     -
#'     n
#'     \mathbf{\bar{Y}}^{2}.
#'   }
#' Note that
#'   \deqn{
#'     TSS
#'     =
#'     ESS
#'     +
#'     RSS.
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams rss
#' @return Returns the total sum of squares.
#' @inherit rss references
#' @family sum of squares functions
#' @export
tss <- function(y) {
  drop(
    crossprod(y) - length(y) * mean(y)^2
  )
}

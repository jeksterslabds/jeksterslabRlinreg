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
#' and `beta_hat` as an optional argument.
#' If `e` is provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param e Numeric vector.
#'   Residuals.
#' @inheritParams e_y_minus_y_hat
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
ss_r_e <- function(e = NULL,
                   beta_hat = NULL,
                   X,
                   y) {
  if (is.null(e)) {
    e <- e(
      X = X,
      y = y,
      beta_hat = beta_hat
    )
  }
  drop(
    crossprod(e)
  )
}

#' Residual Sum of Square
#'
#' @details If `beta_hat = NULL`,
#'   the `beta_hat` vector is computed
#'   using [`beta_hat_inv()`].
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams ss_r_e
#' @inherit ss_r_e description references return
#' @family sum of squares functions
#' @export
ss_r <- function(beta_hat = NULL,
                 X,
                 y) {
  ss_r_e(
    e = NULL,
    beta_hat = beta_hat,
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
#' @inheritParams ss_r
#' @return Returns the explained sum of squares.
#' @inherit ss_r references
#' @family sum of squares functions
#' @export
ss_e <- function(beta_hat = NULL,
                 X,
                 y) {
  if (is.null(beta_hat)) {
    beta_hat <- beta_hat_inv(
      X = X,
      y = y
    )
  }
  drop(
    (t(beta_hat) %*% t(X) %*% X %*% beta_hat) - (nrow(X) * mean(y)^2)
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
#' @inheritParams ss_r
#' @return Returns the total sum of squares.
#' @inherit ss_r references
#' @family sum of squares functions
#' @export
ss_t <- function(y) {
  drop(
    crossprod(y) - length(y) * mean(y)^2
  )
}

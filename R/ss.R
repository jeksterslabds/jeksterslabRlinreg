#' Residual Sum of Square
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
#' If `beta_hat = NULL`,
#' the `beta_hat` vector is computed
#' using [`beta_hat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
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
#' @export
ss_r <- function(beta_hat = NULL,
                 X,
                 y) {
  drop(
    crossprod(
      e(
        X = X,
        y = y,
        beta_hat = beta_hat
      )
    )
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

#' R-square
#'
#' Calculates the coefficient of determination
#'   \deqn{
#'     R^2
#'     =
#'     1 - \frac{\textrm{Residual sum of squares}}{\textrm{Total sum of squares}}
#'   }
#'   or
#'   \deqn{R^2 = \frac{\textrm{Explained sum of squares}}{\textrm{Total sum of squares}}.
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams ss_r
#' @param rss Logical.
#'   If \code{TRUE}, the function uses the residual sum of squares in the calculation.
#'   If \code{FALSE}, the function uses the estimated sum of squares in the calculation.
#' @return Returns the coefficient of determination \eqn{R^2}.
#' @inherit ss_r references
#' @family assessment of model quality functions
#' @export
r_sqr <- function(beta_hat = NULL,
                  X,
                  y,
                  rss = TRUE) {
  tss <- ss_t(y = y)
  if (rss) {
    rss <- ss_r(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
    return(1 - (rss / tss))
  } else {
    ess <- ss_e(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
    return(ess / tss)
  }
}

#' Adjusted R-square
#'
#' Calculates the adjusted coefficient of determination
#'   \deqn{
#'     \bar{R}^{2}
#'     =
#'     1
#'     -
#'     \left(
#'       \frac{RSS / \left( n - k \right)}{TSS / \left(n - 1 \right)}
#'     \right)
#'     =
#'     1
#'     -
#'     \left(
#'       1 - R^2
#'     \right)
#'     \frac{n - 1}{n - k}.
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors
#'   including a regressor
#'   whose value is 1 for each observation.
#' @param r2 Coefficient of determination \eqn{R^2}.
#' @param ... Arguments to pass to [`r_sqr()`] if `r2 = NULL`.
#' @return Returns the adjusted coefficient of determination \eqn{\bar{R}^{2}}.
#' @inherit ss_r references
#' @family assessment of model quality functions
#' @export
r_bar_sqr <- function(r2 = NULL,
                      n,
                      k,
                      ...) {
  if (is.null(r2)) {
    r2 <- r_sqr(
      ...
    )
  }
  return(
    1 - (1 - r2) * ((n - 1) / (n - k))
  )
}

#' Mean Square Error
#'
#' Calculates the mean square error (MSE) using
#'   \deqn{
#'     MSE
#'     =
#'     \frac{1}{n}
#'     \sum_{i = 1}^{n}
#'     \left(
#'       Y_i - \hat{Y}_{i}
#'     \right)^{2}
#'     =
#'     \frac{RSS}{n}.
#'   }
#'
#' If `rss = NULL`,
#' the `rss` is computed
#' using [`ss_r()`].
#' In this case the arguments
#' `X`,
#' `y`, and
#' the optional argument `beta_hat`
#' are supplied using the `...` argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param rss Numeric.
#'   Residual sum of squares.
#' @param n Integer.
#'   Sample size.
#' @param ... Arguments to pass to [`ss_r()`]
#'   if `rss = NULL`.
#' @references
#'   [Wikipedia: Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error)
#' @family assessment of model quality functions
#' @export
mse <- function(rss = NULL,
                n,
                ...) {
  if (is.null(rss)) {
    rss <- ss_r(
      ...
    )
  }
  rss / n
}

#' Root Mean Square Error
#'
#' Calculates the root mean square error (RMSE) using
#'   \deqn{
#'     RMSE
#'     =
#'     \sqrt{
#'       \frac{1}{n}
#'       \sum_{i = 1}^{n}
#'       \left(
#'         Y_i - \hat{Y}_{i}
#'       \right)^{2}
#'     }
#'     =
#'     \sqrt{\frac{RSS}{n}}.
#'   }
#'
#' If `rss = NULL`,
#' the `rss` is computed
#' using [`ss_r()`].
#' In this case the arguments
#' `X`,
#' `y`, and
#' the optional argument `beta_hat`
#' are supplied using the `...` argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams mse
#' @references
#'   [Wikipedia: Root Mean Square Deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
#' @family assessment of model quality functions
#' @export
rmse <- function(rss = NULL,
                 n,
                 ...) {
  sqrt(
    mse(
      rss = rss,
      n = n,
      ...
    )
  )
}

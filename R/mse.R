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

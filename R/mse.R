#' Mean Squared Error (from \eqn{RSS})
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
#' the `rss` vector is computed
#' using [`rss()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `rss` is provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .rbar2
#' @inheritParams .r2_rss
#' @references
#'   [Wikipedia: Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error)
#' @family assessment of model quality functions
#' @return Returns the mean square error.
#' @export
.mse <- function(rss = NULL,
                 n,
                 betahat = NULL,
                 X,
                 y) {
  if (is.null(rss)) {
    rss <- rss(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  rss / n
}

#' Mean Squared Error
#'
#' @inheritParams .mse
#' @inherit .mse description references return
#' @family assessment of model quality functions
#' @export
mse <- function(betahat = NULL,
                X,
                y) {
  .mse(
    rss = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

#' Root Mean Squared Error (from \eqn{RSS})
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
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .mse
#' @inherit .mse details
#' @references
#'   [Wikipedia: Root Mean Square Deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
#' @return Returns the root mean square error.
#' @family assessment of model quality functions
#' @export
.rmse <- function(rss = NULL,
                  n,
                  betahat = NULL,
                  X,
                  y) {
  if (is.null(rss)) {
    rss <- rss(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  sqrt(rss / n)
}

#' Root Mean Squared Error
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .rmse
#' @inherit .rmse description details return references
#' @family assessment of model quality functions
#' @export
rmse <- function(betahat = NULL,
                 X,
                 y) {
  .rmse(
    rss = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

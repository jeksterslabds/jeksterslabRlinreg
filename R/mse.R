#' Mean Square Error (from \eqn{RSS})
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
#' using [`ss_r()`]
#' with `X` and `y` as required arguments
#' and `beta_hat` as an optional argument.
#' If `rss` is provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams rbar2_r2
#' @inheritParams r2_rss
#' @references
#'   [Wikipedia: Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error)
#' @family assessment of model quality functions
#' @return Returns the mean square error.
#' @export
mse_rss <- function(rss = NULL,
                    n,
                    beta_hat = NULL,
                    X,
                    y) {
  if (is.null(rss)) {
    rss <- ss_r(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  rss / n
}

#' Mean Square
#'
#' @inheritParams mse_rss
#' @inherit mse_rss description references return
#' @family assessment of model quality functions
#' @export
mse <- function(beta_hat = NULL,
                X,
                y) {
  mse_rss(
    rss = NULL,
    beta_hat = beta_hat,
    X = X,
    y = y
  )
}

#' Root Mean Square Error (from \eqn{RSS})
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
#' @inheritParams mse_rss
#' @inherit mse_rss details
#' @references
#'   [Wikipedia: Root Mean Square Deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
#' @return Returns the root mean square error.
#' @family assessment of model quality functions
#' @export
rmse_rss <- function(rss = NULL,
                     n,
                     beta_hat = NULL,
                     X,
                     y) {
  if (is.null(rss)) {
    rss <- ss_r(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  sqrt(rss / n)
}

#' Root Mean Square Error
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams rmse_rss
#' @inherit rmse_rss decription details return references
#' @family assessment of model quality functions
#' @export
rmse <- function(beta_hat = NULL,
                 X,
                 y) {
  rmse_rss(
    rss = NULL,
    beta_hat = beta_hat,
    X = X,
    y = y
  )
}

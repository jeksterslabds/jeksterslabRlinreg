#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Mean Squared Error (from \eqn{RSS})
#'
#' @description Calculates the mean squared error \eqn{\left( MSE \right)} using
#'   \deqn{
#'     MSE  = \frac{1}{n} \sum_{i = 1}^{n}
#'     \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2} \\
#'     = \frac{1}{n} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2} \\
#'     = \frac{RSS}{n} .
#'   }
#'
#' @details If `RSS = NULL`, the `RSS` vector is computed using [`RSS()`]
#'   with `X` and `y` as required arguments and `betahat` as an optional argument.
#'   If `RSS` is provided, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .Rbar2
#' @inheritParams .R2fromRSS
#' @return Returns the mean squared error.
#' @references
#' [Wikipedia: Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error)
#' @export
.MSE <- function(RSS = NULL,
                 n,
                 X,
                 y) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  RSS / n
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Mean Squared Error
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .MSE
#' @inherit .MSE description references return
#' @export
MSE <- function(X,
                y) {
  .MSE(
    RSS = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Root Mean Squared Error (from \eqn{RSS})
#'
#' @description Calculates the root mean squared error \eqn{\left( RMSE \right)} using
#'   \deqn{
#'     RMSE = \sqrt{\frac{1}{n} \sum_{i = 1}^{n}
#'     \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2}} \\
#'     = \sqrt{\frac{1}{n} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2}} \\
#'     = \sqrt{\frac{RSS}{n}} .
#'   }
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .MSE
#' @inherit .MSE details
#' @param MSE Numeric.
#'   Mean square error.
#' @return Returns the root mean squared error.
#' @references
#' [Wikipedia: Root Mean Square Deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
#' @export
.RMSE <- function(MSE = NULL,
                  X,
                  y) {
  if (is.null(MSE)) {
    MSE <- MSE(
      X = X,
      y = y
    )
  }
  sqrt(MSE)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Root Mean Squared Error
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .RMSE
#' @inherit .RMSE description details return references
#' @export
RMSE <- function(X,
                 y) {
  .RMSE(
    MSE = NULL,
    X = X,
    y = y
  )
}

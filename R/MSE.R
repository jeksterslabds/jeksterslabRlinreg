#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Mean Squared Error (from \eqn{\mathrm{RSS}})
#'
#' @description Calculates the mean squared error
#'   \eqn{\left( \mathrm{MSE} \right)} using
#'   \deqn{
#'     \mathrm{MSE}
#'     = \frac{1}{n} \sum_{i = 1}^{n}
#'       \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2} \\
#'     = \frac{1}{n} \sum_{i = 1}^{n}
#'       \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2} \\
#'     = \frac{\mathrm{RSS}}{n} .
#'   }
#'
#' @details If `RSS = NULL`, the `RSS` vector is computed using [`RSS()`]
#'   with `X` and `y` as required arguments.
#'   If `RSS` is provided, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .Rbar2
#' @inheritParams .R2fromRSS
#' @return Returns the mean squared error.
#' @references
#' [Wikipedia: Mean squared error](https://en.wikipedia.org/wiki/Mean_squared_error)
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
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' MSE(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' MSE(X = X, y = y)
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
#' @title Root Mean Squared Error (from \eqn{\mathrm{RSS}})
#'
#' @description Calculates the root mean squared error
#'   \eqn{\left( \mathrm{RMSE} \right)} using
#'   \deqn{
#'     \mathrm{RMSE}
#'     = \sqrt{\frac{1}{n} \sum_{i = 1}^{n}
#'       \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2}} \\
#'     = \sqrt{\frac{1}{n} \sum_{i = 1}^{n}
#'       \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2}} \\
#'     = \sqrt{\frac{\mathrm{RSS}}{n}} .
#'   }
#'
#' @details If `MSE = NULL`, `MSE` is computed using [`MSE()`]
#'   with `X` and `y` as required arguments.
#'   If `MSE` is provided, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .MSE
#' @inherit .MSE details
#' @param MSE Numeric.
#'   Mean square error.
#' @return Returns the root mean squared error.
#' @references
#' [Wikipedia: Root-mean-square deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
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
#' @inherit .RMSE description return references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' RMSE(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' RMSE(X = X, y = y)
#' @export
RMSE <- function(X,
                 y) {
  .RMSE(
    MSE = NULL,
    X = X,
    y = y
  )
}

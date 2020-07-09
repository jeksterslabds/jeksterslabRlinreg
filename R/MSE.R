#' Mean Squared Error
#' (from
#' \eqn{
#'   RSS
#' }
#' )
#'
#' Calculates the mean squared error
#' \eqn{
#'   \left(
#'     MSE
#'   \right)
#' }
#' using
#' \deqn{
#'   MSE
#'   =
#'   \frac{
#'     1
#'   }
#'   {
#'     n
#'   }
#'   \sum_{
#'     i
#'     =
#'     1
#'   }^{
#'     n
#'   }
#'   \left(
#'     \mathbf{
#'       y
#'     }
#'     -
#'     \mathbf{
#'       X
#'     }
#'     \boldsymbol{
#'       \hat{
#'         \beta
#'       }
#'     }
#'     \right)^{
#'       2
#'     } \\
#'     =
#'     \frac{
#'       1
#'     }
#'     {
#'       n
#'     }
#'     \sum_{
#'       i
#'       =
#'       1
#'     }^{
#'       n
#'     }
#'     \left(
#'       \mathbf{
#'         y
#'       }
#'       -
#'       \mathbf{
#'         \hat{
#'           y
#'         }
#'       }
#'     \right)^{
#'       2
#'     } \\
#'     =
#'     \frac{
#'       RSS
#'     }
#'     {
#'       n
#'     } .
#'   }
#'
#' If `RSS = NULL`,
#' the `RSS` vector is computed
#' using [`RSS()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `RSS` is provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family assessment of model quality functions
#' @keywords mean square error
#' @inheritParams .Rbar2
#' @inheritParams .R2_RSS
#' @return Returns the mean squared error.
#' @references
#' [Wikipedia: Mean Squared Error](https://en.wikipedia.org/wiki/Mean_squared_error)
#' @export
.MSE <- function(RSS = NULL,
                 n,
                 X,
                 y,
                 betahat = NULL) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y,
      betahat = betahat
    )
    n <- nrow(X)
  }
  RSS / n
}

#' Mean Squared Error
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family assessment of model quality functions
#' @keywords mean square error
#' @inheritParams .MSE
#' @inherit .MSE description references return
#' @export
MSE <- function(X,
                y,
                betahat = NULL) {
  .MSE(
    RSS = NULL,
    X = X,
    y = y,
    betahat = betahat
  )
}

#' Root Mean Squared Error
#' (from
#' \eqn{
#'   RSS
#' }
#' )
#'
#' Calculates the root mean squared error
#' \eqn{
#'   \left(
#'     RMSE
#'   \right)
#' }
#' using
#' \deqn{
#'   RMSE
#'   =
#'   \sqrt{
#'     \frac{
#'       1
#'     }
#'     {
#'       n
#'     }
#'     \sum_{
#'       i
#'       =
#'       1
#'     }^{
#'       n
#'     }
#'     \left(
#'       \mathbf{
#'         y
#'       }
#'       -
#'       \mathbf{
#'         X
#'       }
#'       \boldsymbol{
#'         \hat{
#'           \beta
#'         }
#'       }
#'     \right)^{
#'       2
#'     }
#'   } \\
#'   =
#'   \sqrt{
#'     \frac{
#'       1
#'     }
#'     {
#'       n
#'     }
#'     \sum_{
#'       i
#'       =
#'       1
#'     }^{
#'       n
#'     }
#'     \left(
#'       \mathbf{
#'         y
#'       }
#'       -
#'       \mathbf{
#'         \hat{
#'           y
#'         }
#'       }
#'     \right)^{
#'       2
#'     }
#'   } \\
#'   =
#'   \sqrt{
#'     \frac{
#'       RSS
#'     }
#'     {
#'       n
#'     }
#'   } .
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family assessment of model quality functions
#' @keywords mean square error
#' @inheritParams .MSE
#' @inherit .MSE details
#' @return Returns the root mean squared error.
#' @references
#' [Wikipedia: Root Mean Square Deviation](https://en.wikipedia.org/wiki/Root-mean-square_deviation)
#' @export
.RMSE <- function(RSS = NULL,
                  n,
                  X,
                  y,
                  betahat = NULL) {
  if (is.null(RSS)) {
    RSS <- RSS(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  sqrt(RSS / n)
}

#' Root Mean Squared Error
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family assessment of model quality functions
#' @keywords mean square error
#' @inheritParams .RMSE
#' @inherit .RMSE description details return references
#' @export
RMSE <- function(X,
                 y,
                 betahat = NULL) {
  .RMSE(
    RSS = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

#' Residual Sum of Square (from \eqn{\boldsymbol{\hat{\varepsilon}}})
#'
#' Calculates the residual sum of squares (RSS) using
#'   \deqn{
#'     RSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       Y_i
#'       -
#'       \hat{Y}_i
#'     \right)^2 \\
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       Y_i
#'       -
#'       \left[
#'         \hat{\beta}_{1}
#'         +
#'         \hat{\beta}_{2}
#'         X_{2i}
#'         +
#'         \hat{\beta}_{3}
#'         X_{3i}
#'         +
#'         \dots
#'         +
#'         \hat{\beta}_{k}
#'         X_{ki}
#'       \right]
#'     \right)^2 \\
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       Y_i
#'       -
#'       \hat{\beta}_{1}
#'       -
#'       \hat{\beta}_{2}
#'       X_{2i}
#'       -
#'       \hat{\beta}_{3}
#'       X_{3i}
#'       -
#'       \dots
#'       -
#'       \hat{\beta}_{k}
#'       X_{ki}
#'     \right)^2 .
#'   }
#' In matrix form
#'   \deqn{
#'     RSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{\hat{y}}
#'     \right)^{2} \\
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{X}
#'       \boldsymbol{\hat{\beta}}
#'     \right)^{2} \\
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
#'     \right) .
#'   }
#' Or simply
#'   \deqn{
#'     RSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \boldsymbol{\hat{\varepsilon}}_{i}^{2}
#'     =
#'     \boldsymbol{\hat{\varepsilon}}^{\prime}
#'     \boldsymbol{\hat{\varepsilon}}
#'   }
#' where
#' \eqn{\boldsymbol{\hat{\varepsilon}}}
#' is an \eqn{n \times 1}
#' vector of residuals,
#' that is,
#' the difference between
#' the observed and predicted value of \eqn{\mathbf{y}}
#' (\eqn{\boldsymbol{\hat{\varepsilon}} = \mathbf{y}} - \eqn{\mathbf{\hat{y}}}).
#' Equivalent computational matrix formula
#'   \deqn{
#'     RSS
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
#' If `epsilonhat = NULL`,
#' RSS is computed with
#' `X` and `y`
#' as required arguments
#' and
#' `betahat`
#' as an optional argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param epsilonhat Numeric vector.
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
#' @return Returns residual sum of squares (\eqn{RSS}).
#' @export
.RSS <- function(epsilonhat = NULL,
                 X,
                 y,
                 betahat = NULL) {
  if (is.null(epsilonhat)) {
    if (is.null(betahat)) {
      betahat <- betahat_inv(
        X = X,
        y = y
      )
    }
    return(
      drop(
        crossprod(y) - (2 * (betahat %*% crossprod(X, y))) + (t(betahat) %*% t(X) %*% X %*% betahat)
      )
    )
  } else {
    return(
      drop(
        crossprod(epsilonhat)
      )
    )
  }
}

#' Residual Sum of Square (from raw data)
#'
#' @details If `betahat = NULL`,
#'   `betahat` computed
#'   using [`betahat_inv()`].
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .RSS
#' @inherit .RSS description references return
#' @family sum of squares functions
#' @export
.RSS_raw <- function(X,
                     y,
                     betahat = NULL) {
  if (is.null(betahat)) {
    betahat <- betahat_inv(
      X = X,
      y = y
    )
  }
  drop(
    crossprod(y) - (2 * (betahat %*% crossprod(X, y))) + (t(betahat) %*% t(X) %*% X %*% betahat)
  )
}

#' Residual Sum of Square
#'
#' @details If `betahat = NULL`,
#'   `betahat` computed
#'   using [`betahat_inv()`].
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .RSS
#' @inherit .RSS description references return
#' @family sum of squares functions
#' @export
RSS <- function(X,
                y,
                betahat = NULL) {
  .RSS(
    epsilonhat = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

#' Explained Sum of Squares (from \eqn{\mathbf{\hat{y}}} and \eqn{\mathbf{\bar{Y}}})
#'
#' Calculates the explained sum of squares (ESS)
#' using
#'   \deqn{
#'     ESS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \hat{Y}_{i}
#'       -
#'       \bar{Y}
#'     \right)^2 \\
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \hat{\beta}_{1}
#'       +
#'       \hat{\beta}_{2}
#'       X_{2i}
#'       +
#'       \hat{\beta}_{3}
#'       X_{3i}
#'       +
#'       \dots
#'       +
#'       \hat{\beta}_{k}
#'       X_{ki}
#'       -
#'       \bar{Y}
#'     \right)^2 \\
#'   }
#' In matrix form
#'   \deqn{
#'     ESS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{\hat{y}}
#'       -
#'       \mathbf{\bar{Y}}
#'     \right)^2 \\
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{X}
#'       \boldsymbol{\hat{\beta}}
#'       -
#'       \mathbf{\bar{Y}}
#'     \right)^2
#'     }
#' where
#' \eqn{\mathbf{\hat{y}}}
#' (\eqn{\mathbf{X} \boldsymbol{\hat{\beta}}})
#' is an \eqn{n \times 1} matrix
#' of predicted values of
#' \eqn{\mathbf{y}},
#' and
#' \eqn{\mathbf{\bar{Y}}}
#' is the mean of
#' \eqn{\mathbf{y}}.
#' Equivalent computational matrix formula
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
#' @details If `yhat = NULL`,
#' it is computed using [`yhat()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param ybar Numeric.
#'   Mean of y.
#' @inheritParams y_minus_yhat
#' @return Returns the explained sum of squares (\eqn{ESS}).
#' @inherit RSS references
#' @family sum of squares functions
#' @export
.ESS <- function(yhat = NULL,
                 ybar = NULL,
                 X,
                 y,
                 betahat = NULL) {
  if (is.null(ybar)) {
    ybar <- mean(y)
  }
  if (is.null(yhat)) {
    yhat <- yhat(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  drop(
    sum(yhat^2) - (2 * ybar * sum(yhat)) + (length(as.vector(yhat)) * ybar^2)
  )
}

#' Explained Sum of Squares (from raw data)
#'
#' @details If `betahat = NULL`,
#' it is computed using [`betahat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .ESS
#' @inherit .ESS description references return
#' @family sum of squares functions
#' @export
.ESS_raw <- function(X,
                     y,
                     betahat = NULL) {
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

#' Explained Sum of Squares
#'
#' @details If `betahat = NULL`,
#' it is computed using [`betahat_inv()`].
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .ESS
#' @inherit .ESS description references return
#' @family sum of squares functions
#' @export
ESS <- function(X,
                y,
                betahat = NULL) {
  # if (is.null(betahat)) {
  #  betahat <- betahat_inv(
  #    X = X,
  #    y = y
  #  )
  # }
  # drop(
  #  (t(betahat) %*% t(X) %*% X %*% betahat) - (nrow(X) * mean(y)^2)
  # )
  .ESS(
    yhat = NULL,
    ybar = NULL,
    X = X,
    y = y,
    betahat = betahat
  )
}

#' Total Sum of Squares.
#'
#' Calculates the total sum of squares (TSS)
#' using
#'   \deqn{
#'     TSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       Y_i
#'       -
#'       \bar{Y}
#'     \right)^2 \\
#'     =
#'     \sum_{i = 1}^{n}
#'     Y_{i}^{2}
#'     -
#'     n
#'     \bar{Y}^2
#'   }
#' In matrix form
#'   \deqn{
#'     TSS
#'     =
#'     \sum_{i = 1}^{n}
#'     \left(
#'       \mathbf{y}
#'       -
#'       \mathbf{\bar{y}}
#'     \right)^2
#'   }
#' Equivalent computational matrix formula
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
#' @inheritParams RSS
#' @return Returns the total sum of squares (\eqn{TSS}).
#' @inherit RSS references
#' @family sum of squares functions
#' @export
TSS <- function(y) {
  drop(
    crossprod(y) - length(y) * mean(y)^2
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Sum of Square (from \eqn{\boldsymbol{\hat{\varepsilon}}})
#'
#' @description Calculates the residual sum of squares \eqn{\left( \mathrm{RSS} \right)} using
#'   \deqn{
#'     \mathrm{RSS} = \sum_{i = 1}^{n} \left( Y_i - \hat{Y}_i \right)^2 \\
#'     = \sum_{i = 1}^{n} \left( Y_i -
#'     \left[ \hat{\beta}_{1} + \hat{\beta}_{2} X_{2i} + \hat{\beta}_{3} X_{3i} +
#'     \dots + \hat{\beta}_{k} X_{ki} \right] \right)^2 \\
#'     = \sum_{i = 1}^{n} \left( Y_i - \hat{\beta}_{1} - \hat{\beta}_{2} X_{2i}
#'     - \hat{\beta}_{3} X_{3i} - \dots - \hat{\beta}_{k} X_{ki} \right)^2 .
#'   }
#'   In matrix form
#'   \deqn{
#'     \mathrm{RSS} = \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{\hat{y}} \right)^{2} \\
#'     = \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{2} \\
#'     = \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^{\prime}
#'     \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right) .
#'   }
#'   Or simply
#'   \deqn{
#'     \mathrm{RSS} = \sum_{i = 1}^{n} \boldsymbol{\hat{\varepsilon}}_{i}^{2}
#'     = \boldsymbol{\hat{\varepsilon}}^{\prime} \boldsymbol{\hat{\varepsilon}}
#'   }
#'   where
#'   \eqn{\boldsymbol{\hat{\varepsilon}}} is an \eqn{n \times 1} vector of residuals,
#'   that is, the difference between the observed and predicted value of \eqn{\mathbf{y}}
#'   \eqn{\left( \boldsymbol{\hat{\varepsilon}} = \mathbf{y} - \mathbf{\hat{y}} \right)}.
#'   Equivalent computational matrix formula
#'   \deqn{
#'     \mathrm{RSS} = \mathbf{y}^{\prime} \mathbf{y} - 2 \boldsymbol{\hat{\beta}} \mathbf{X}^{\prime}
#'     \mathbf{y} + \boldsymbol{\hat{\beta}}^{\prime} \mathbf{X}^{\prime} \mathbf{X}
#'     \boldsymbol{\hat{\beta}}.
#'   }
#'   Note that
#'   \deqn{
#'     \mathrm{TSS} = \mathrm{ESS} + \mathrm{RSS}.
#'   }
#'
#' @details If `epsilonhat = NULL`, \eqn{\left( \mathrm{RSS} \right)} is computed with `X` and `y` as required arguments
#'   and `betahat` as an optional argument.
#'
#' @family sum of squares functions
#' @keywords SS
#' @inheritParams .yminusyhat
#' @param epsilonhat Numeric vector of length `n` or `n` by 1 numeric matrix.
#'   \eqn{n \times 1} vector of residuals.
#' @references
#'   [Wikipedia: Residual Sum of Squares](https://en.wikipedia.org/wiki/Residual_sum_of_squares)
#'
#'   [Wikipedia: Explained Sum of Squares](https://en.wikipedia.org/wiki/Explained_sum_of_squares)
#'
#'   [Wikipedia: Total Sum of Squares](https://en.wikipedia.org/wiki/Total_sum_of_squares)
#'
#'   [Wikipedia: Coefficient of Determination](https://en.wikipedia.org/wiki/Coefficient_of_determination)
#' @return Returns residual sum of squares \eqn{\left( \mathrm{RSS} \right)}.
#' @export
.RSS <- function(epsilonhat = NULL,
                 X,
                 y,
                 betahat = NULL) {
  if (is.null(epsilonhat)) {
    if (is.null(betahat)) {
      betahat <- betahat(
        X = X,
        y = y
      )
    }
    return(
      drop(
        unname(
          crossprod(y) - (2 * (t(betahat) %*% crossprod(X, y))) + (t(betahat) %*% crossprod(X) %*% betahat)
        )
      )
    )
  } else {
    return(
      drop(
        unname(
          crossprod(epsilonhat)
        )
      )
    )
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Sum of Squares
#'
#' @details If `betahat = NULL`, `betahat` computed using [`betahat()`].
#'
#' @family sum of squares functions
#' @keywords SS
#' @inheritParams .RSS
#' @inherit .RSS description references return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' RSS(X = X, y = y)
#' @export
RSS <- function(X,
                y) {
  .RSS(
    epsilonhat = NULL,
    betahat = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Explained Sum of Squares (from \eqn{\mathbf{\hat{y}}} and \eqn{\mathbf{\bar{Y}}})
#'
#' @description Calculates the explained sum of squares \eqn{\left( \mathrm{ESS} \right)}
#'   using
#'   \deqn{
#'     \mathrm{ESS} = \sum_{i = 1}^{n} \left( \hat{Y}_{i} - \bar{Y} \right)^2 \\
#'     = \sum_{i = 1}^{n} \left( \hat{\beta}_{1} + \hat{\beta}_{2} X_{2i} +
#'     \hat{\beta}_{3} X_{3i} + \dots + \hat{\beta}_{k} X_{ki} - \bar{Y} \right)^2
#'   }
#'   In matrix form
#'   \deqn{
#'     \mathrm{ESS} = \sum_{i = 1}^{n} \left( \mathbf{\hat{y}} - \mathbf{\bar{Y}} \right)^2 \\
#'     = \sum_{i = 1}^{n} \left( \mathbf{X} \boldsymbol{\hat{\beta}} - \mathbf{\bar{Y}} \right)^2
#'   }
#'   where
#'   \eqn{\mathbf{\hat{y}}} \eqn{\left( \mathbf{X} \boldsymbol{\hat{\beta}} \right)}
#'   is an \eqn{n \times 1} matrix of predicted values of \eqn{\mathbf{y}},
#'   and \eqn{\mathbf{\bar{Y}}} is the mean of \eqn{\mathbf{y}}.
#'   Equivalent computational matrix formula
#'   \deqn{
#'     \mathrm{ESS} = \boldsymbol{\hat{\beta}}^{\prime} \mathbf{X}^{\prime} \mathbf{X}
#'     \boldsymbol{\hat{\beta}} - n \mathbf{\bar{Y}}^{2}.
#'   }
#'   Note that
#'   \deqn{
#'     \mathrm{TSS} = \mathrm{ESS} + \mathrm{RSS} .
#'   }
#'
#' @details If `yhat = NULL`, it is computed using [`yhat()`]
#'   with `X` and `y` as required arguments and `betahat` as an optional argument.
#'
#' @family sum of squares functions
#' @keywords SS
#' @inherit RSS references
#' @param ybar Numeric.
#'   Mean of `y`.
#' @inheritParams .yminusyhat
#' @inheritParams .Xbetahat
#' @return Returns the explained sum of squares \eqn{\left( \mathrm{ESS} \right)}.
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
    yhat <- .Xbetahat(
      X = X,
      betahat = betahat,
      y = y
    )
  }
  drop(
    unname(
      sum(yhat^2) - (2 * ybar * sum(yhat)) + (length(as.vector(yhat)) * ybar^2)
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Explained Sum of Squares
#'
#' @family sum of squares functions
#' @keywords SS
#' @inheritParams .ESS
#' @inherit .ESS description references return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' ESS(
#'   X = X,
#'   y = y
#' )
#' @export
ESS <- function(X,
                y) {
  .ESS(
    yhat = NULL,
    ybar = NULL,
    X,
    y,
    betahat = NULL
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Total Sum of Squares.
#'
#' @description Calculates the total sum of squares \eqn{\left( \mathrm{TSS} \right)} using
#'   \deqn{
#'     \mathrm{TSS} = \sum_{i = 1}^{n} \left( Y_i - \bar{Y} \right)^2 \\
#'     = \sum_{i = 1}^{n} Y_{i}^{2} - n \bar{Y}^2
#'   }
#'   In matrix form
#'   \deqn{
#'     \mathrm{TSS} = \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{\bar{y}} \right)^2
#'   }
#'   Equivalent computational matrix formula
#'   \deqn{
#'     \mathrm{TSS} = \mathbf{y}^{\prime} \mathbf{y} - n \mathbf{\bar{Y}}^{2}.
#'   }
#'   Note that
#'   \deqn{
#'     \mathrm{TSS} = \mathrm{ESS} + \mathrm{RSS} .
#'   }
#'
#' @family sum of squares functions
#' @keywords SS
#' @inheritParams RSS
#' @return Returns the total sum of squares \eqn{\left( \mathrm{TSS} \right)}.
#' @inherit RSS references
#' @examples
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' TSS(
#'   y = y
#' )
#' @export
TSS <- function(y) {
  drop(
    unname(
      crossprod(y) - length(y) * mean(y)^2
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\hat{\varepsilon}}^{2}} (from \eqn{\mathrm{RSS}})
#'
#' @description Calculates an estimate of the error variance
#'   \deqn{
#'     \mathbf{E} \left( \sigma^2 \right) = \hat{\sigma}_{\hat{\varepsilon}}^{2}
#'   }
#'   \deqn{
#'     \hat{\sigma}_{\hat{\varepsilon}}^{2}
#'     = \frac{1}{n - k} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\hat{\varepsilon}}^{\prime} \boldsymbol{\hat{\varepsilon}}}{n - k} \\
#'     = \frac{\mathrm{RSS}}{n - k}}
#'   where
#'   \eqn{\boldsymbol{\hat{\varepsilon}}} is the vector of residuals,
#'   \eqn{\mathrm{RSS}} is the residual sum of squares,
#'   \eqn{n} is the sample size,
#'   and \eqn{k} is the number of regressors
#'   including a regressor whose value is 1 for each observation on the first column.
#'
#' @details If `RSS = NULL`, `RSS` is computed using [`RSS()`].
#'   If `RSS` is provided, `X`, and `y` are not needed.
#'
#' @family residual variance functions
#' @keywords sigma2hatepsilonhat
#' @inheritParams yminusyhat
#' @inheritParams RSS
#' @inherit yhat references
#' @param RSS Numeric.
#'   Residual sum of squares.
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors including a regressor
#'   whose value is 1 for each observation on the first column.
#' @return Returns the estimated residual variance \eqn{\hat{\sigma}_{\hat{\varepsilon}}^{2}} .
#' @export
.sigma2hatepsilonhat <- function(RSS = NULL,
                                 n,
                                 k,
                                 X,
                                 y) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  RSS / (n - k)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\hat{\varepsilon} \ \textrm{biased}}^{2}} (from \eqn{\mathrm{RSS}})
#'
#' @description Calculates an estimate of the error variance
#'   \deqn{
#'     \mathbf{E} \left( \sigma^2 \right) = \hat{\sigma}_{\hat{\varepsilon}}^{2}
#'   }
#'   \deqn{
#'     \hat{\sigma}_{\hat{\varepsilon} \ \textrm{biased}}^{2}
#'     = \frac{1}{n} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\hat{\varepsilon}}^{\prime} \boldsymbol{\hat{\varepsilon}}}{n} \\
#'     = \frac{\mathrm{RSS}}{n}
#'   }
#'   where
#'   \eqn{\boldsymbol{\hat{\varepsilon}}} is the vector of residuals,
#'   \eqn{\mathrm{RSS}} is the residual sum of squares, and
#'   \eqn{n} is the sample size.
#'
#' @family residual variance functions
#' @keywords sigma2hatepsilonhat
#' @inheritParams yminusyhat
#' @inheritParams RSS
#' @inheritParams .sigma2hatepsilonhat
#' @inherit yhat references
#' @inherit .sigma2hatepsilonhat details
#' @return Returns the estimated residual variance \eqn{\hat{\sigma}_{\hat{\varepsilon} \ \textrm{biased}}^{2}} .
#' @export
.sigma2hatepsilonhatbiased <- function(RSS = NULL,
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
#' @title Residual Variance \eqn{\hat{\sigma}_{\hat{\varepsilon}}^{2}}
#'
#' @family residual variance functions
#' @keywords sigma2hatepsilonhat
#' @inheritParams .sigma2hatepsilonhat
#' @inherit .sigma2hatepsilonhat return description references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sigma2hatepsilonhat(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sigma2hatepsilonhat(X = X, y = y)
#' @export
sigma2hatepsilonhat <- function(X,
                                y) {
  .sigma2hatepsilonhat(
    RSS = NULL,
    n = NULL,
    k = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\hat{\varepsilon} \ \textrm{biased}}^{2}}
#'
#' @family residual variance functions
#' @keywords sigma2hatepsilonhat
#' @inheritParams .sigma2hatepsilonhatbiased
#' @inherit .sigma2hatepsilonhatbiased return description references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sigma2hatepsilonhatbiased(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sigma2hatepsilonhatbiased(X = X, y = y)
#' @export
sigma2hatepsilonhatbiased <- function(X,
                                      y) {
  .sigma2hatepsilonhatbiased(
    RSS = NULL,
    n = NULL,
    X = X,
    y = y
  )
}

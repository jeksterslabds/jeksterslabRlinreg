#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\varepsilon}^{2}} (from \eqn{RSS})
#'
#' @description Calculates an estimate of the error variance
#'   \deqn{
#'     \mathbf{E} \left( \sigma^2 \right) = \hat{\sigma}_{\varepsilon}^{2}
#'   }
#'   \deqn{
#'     \hat{\sigma}_{\varepsilon}^{2}
#'     = \frac{1}{n - k} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\varepsilon}^{\prime} \boldsymbol{\varepsilon}}{n - k} \\
#'     = \frac{RSS}{n - k}}
#'   where
#'   \eqn{\boldsymbol{\varepsilon}} is the vector of residuals,
#'   \eqn{RSS} is the residual sum of squares,
#'   \eqn{n} is the sample size,
#'   and \eqn{k} is the number of regressors
#'   including a regressor whose value is 1 for each observation on the first column.
#'
#' @details If `RSS = NULL`, `RSS` is computed using [`RSS()`].
#'   If `RSS` is provided, `X`, and `y` are not needed.
#'
#' @family residual variance functions
#' @keywords sigma2epsilonhat
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
#' @return Returns the estimated residual variance \eqn{\hat{\sigma}_{\varepsilon}^{2}} .
#' @export
.sigma2epsilonhat <- function(RSS = NULL,
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
#' @title Residual Variance \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}} (from \eqn{RSS})
#'
#' @description Calculates an estimate of the error variance
#'   \deqn{
#'     \mathbf{E} \left( \sigma^2 \right) = \hat{\sigma}_{\varepsilon}^{2}
#'   }
#'   \deqn{
#'     \hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}
#'     = \frac{1}{n} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\varepsilon}^{\prime} \boldsymbol{\varepsilon}}{n} \\
#'     = \frac{RSS}{n}
#'   }
#'   where
#'   \eqn{\boldsymbol{\varepsilon}} is the vector of residuals,
#'   \eqn{RSS} is the residual sum of squares, and
#'   \eqn{n} is the sample size.
#'
#' @family residual variance functions
#' @keywords sigma2epsilonhat
#' @inheritParams yminusyhat
#' @inheritParams RSS
#' @inheritParams .sigma2epsilonhat
#' @inherit yhat references
#' @inherit .sigma2epsilonhat details
#' @return Returns the estimated residual variance \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}} .
#' @export
.sigma2epsilonhatbiased <- function(RSS = NULL,
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
#' @title Residual Variance \eqn{\hat{\sigma}_{\varepsilon}^{2}}
#'
#' @family residual variance functions
#' @keywords sigma2epsilonhat
#' @inheritParams .sigma2epsilonhat
#' @inherit .sigma2epsilonhat return description references
#' @export
sigma2epsilonhat <- function(X,
                             y) {
  .sigma2epsilonhat(
    RSS = NULL,
    n = NULL,
    k = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}}
#'
#' @family residual variance functions
#' @keywords sigma2epsilonhat
#' @inheritParams .sigma2epsilonhatbiased
#' @inherit .sigma2epsilonhatbiased return description references
#' @export
sigma2epsilonhatbiased <- function(X,
                                   y) {
  .sigma2epsilonhatbiased(
    RSS = NULL,
    n = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Variance \eqn{\hat{\sigma}_{\varepsilon}^{2}} (from \eqn{RSS})
#'
#' @description Calculates estimates of the error variance
#'   \deqn{
#'     \mathbf{E} \left( \sigma^2 \right) = \hat{\sigma}_{\varepsilon}^{2}
#'   }
#'   \deqn{
#'     \hat{\sigma}_{\varepsilon \textrm{unbiased}}^{2}
#'     = \frac{1}{n - k} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\varepsilon}^{\prime} \boldsymbol{\varepsilon}}{n - k} \\
#'     = \frac{RSS}{n - k}}
#'   or
#'   \deqn{
#'     \hat{\sigma}_{\varepsilon \textrm{biased}}^{2}
#'     = \frac{1}{n} \sum_{i = 1}^{n} \left( \mathbf{y} - \mathbf{X} \boldsymbol{\hat{\beta}} \right)^2 \\
#'     = \frac{\boldsymbol{\varepsilon}^{\prime} \boldsymbol{\varepsilon}}{n} \\
#'     = \frac{RSS}{n}
#'   }
#'   where
#'   \eqn{\boldsymbol{\varepsilon}} is the vector of residuals,
#'   \eqn{RSS} is the residual sum of squares,
#'   \eqn{n} is the sample size,
#'   and \eqn{k} is the number of regressors
#'   including a regressor whose value is 1 for each observation.
#'
#' @details If `RSS = NULL`, `RSS` is computed using [`RSS()`]
#'   with `X` and `y` as a required arguments and `betahat` as an optional argument.
#'   If `RSS` is provided, `betahat`, `X`, and `y` are not needed.
#'
#' @family residual variance functions
#' @keywords sigma2epsilonhat
#' @inheritParams yminusyhat
#' @inheritParams .RSS
#' @inherit yhat references
#' @param RSS Numeric.
#'   Residual sum of squares.
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors including a regressor
#'   whose value is 1 for each observation.
#' @param type String.
#'   Residual variance estimator.
#'   If `type = "unbiased"`, returns unbiased estimate.
#'   If `type = "biased"`, returns biased estimate.
#'   If `type = "both"`, returns a vector of unbiased and biased estimates.
#' @return Returns the estimated residual variance \eqn{\hat{\sigma}_{\varepsilon}^{2}} .
#' @export
.sigma2epsilonhat <- function(RSS = NULL,
                              n,
                              k,
                              type = "unbiased",
                              epsilonhat = NULL,
                              X = NULL,
                              y = NULL,
                              betahat = NULL) {
  if (!is.null(X) & !is.null(y)) {
    n <- nrow(X)
    k <- ncol(X)
  }
  if (is.null(RSS)) {
    RSS <- .RSS(
      epsilonhat = epsilonhat,
      X = X,
      y = y,
      betahat = betahat
    )
  }
  unbiased <- RSS / (n - k)
  biased <- RSS / n
  if (type == "unbiased") {
    return(unbiased)
  }
  if (type == "biased") {
    return(biased)
  }
  if (type == "both") {
    return(
      c(
        unbiased = unbiased,
        biased = biased
      )
    )
  }
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
                             y,
                             type = "unbiased") {
  .sigma2epsilonhat(
    RSS = NULL,
    type = type,
    betahat = NULL,
    X = X,
    y = y
  )
}

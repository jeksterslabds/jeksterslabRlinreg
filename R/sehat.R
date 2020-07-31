#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{unbiased}}^{2}})
#'
#' @description \eqn{\widehat{\mathbf{se}}} is equal to the square root of the diagonal elements
#'   of \eqn{\widehat{\mathrm{cov}} \left( \boldsymbol{\hat{\beta}} \right)} .
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams betahat
#' @param vcovhatbetahat `k` by `k` matrix.
#'   \eqn{k \times k} variance-covariance matrix of estimates of regression coefficients.
#' @return Returns the estimated standard errors of the estimated regression coefficients.
#' @export
.sehatbetahat <- function(vcovhatbetahat = NULL,
                          X,
                          y) {
  if (is.null(vcovhatbetahat)) {
    vcovhatbetahat <- vcovhatbetahat(
      X = X,
      y = y
    )
  }
  sqrt(
    diag(
      vcovhatbetahat
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{unbiased}}^{2}})
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatbetahat
#' @inherit .sehatbetahat
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatbetahat(
#'   X = X,
#'   y = y
#' )
#' @export
sehatbetahat <- function(X,
                         y) {
  .sehatbetahat(
    vcovhatbetahat = NULL,
    X = X,
    y = y
  )
}
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams betahat
#' @param vcovhatbetahatbiased `k` by `k` matrix.
#'   \eqn{k \times k} biased variance-covariance matrix of estimates of regression coefficients.
#' @export
.sehatbetahatbiased <- function(vcovhatbetahatbiased = NULL,
                                X,
                                y) {
  if (is.null(vcovhatbetahatbiased)) {
    vcovhatbetahatbiased <- vcovhatbetahatbiased(
      X = X,
      y = y
    )
  }
  as.vector(
    sqrt(
      diag(
        vcovhatbetahatbiased
      )
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{biased}}^{2}})
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatbetahatbiased
#' @inherit .sehatbetahatbiased
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatbetahatbiased(
#'   X = X,
#'   y = y
#' )
#' @export
sehatbetahatbiased <- function(X,
                               y) {
  .sehatbetahatbiased(
    vcovhatbetahatbiased = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Biased)
#'
#' @description \deqn{
#'     \mathrm{\widehat{se}}_{\hat{\beta}^{\prime}} =
#'     \mathrm{\widehat{se}}_{\hat{\beta}} \frac{\hat{\beta}^{\prime}}{\hat{\beta}}
#'   }
#'   According to Yuan and Chan (2011), this standard error is biased.
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .Xbetahat
#' @param sehatbetahat Numeric vector of length `k` or `k` by `1` matrix.
#'   Standard errors of standardized estimates of regression coefficients.
#' @param betahatprime Numeric vector of length `k` or `k` by `1` matrix.
#'   The vector \eqn{\boldsymbol{\hat{\beta}^{\prime}}} is a \eqn{k \times 1} vector of standardized estimates
#'   of \eqn{k} unknown regression coefficients.
#'   The first item (intercept) is equal to zero.
#' @export
.sehatbetahatprimebiased <- function(betahat = NULL,
                                     sehatbetahat = NULL,
                                     betahatprime = NULL,
                                     X,
                                     y) {
  if (is.null(betahat)) {
    betahat <- betahat(
      X = X,
      y = y
    )
  }
  if (is.null(betahatprime)) {
    slopesprime <- slopesprime(
      X = X,
      y = y
    )
    betahatprime <- c(0, slopesprime)
  }
  if (is.null(sehatbetahat)) {
    sehatbetahat <- sehatbetahat(
      X = X,
      y = y
    )
  }
  as.vector(
    sehatbetahat * (betahatprime / betahat)
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatbetahatprimebiased
#' @inherit .sehatbetahatprimebiased description
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatbetahatprimebiased(
#'   X = X,
#'   y = y
#' )
#' @export
sehatbetahatprimebiased <- function(X,
                                    y) {
  .sehatbetahatprimebiased(
    sehatbetahat = NULL,
    betahat = NULL,
    betahatprime = NULL,
    X = X,
    y = y
  )
}

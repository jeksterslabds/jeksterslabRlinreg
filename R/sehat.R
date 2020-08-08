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
  out <- sqrt(
    diag(
      vcovhatbetahat
    )
  )
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatbetahat"
  out
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
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatbetahat(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatbetahat(X = X, y = y)
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
  out <- sqrt(
    diag(
      vcovhatbetahatbiased
    )
  )
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatbetahat"
  out
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
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatbetahatbiased(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatbetahatbiased(X = X, y = y)
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
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Textbook)
#'
#' @description \deqn{
#'     \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}} =
#'     \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{2, \cdots, k}} \frac{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}}{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
#'   }
#'   According to Yuan and Chan (2011), this standard error is biased.
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .Xbetahat
#' @param sehatslopes Numeric vector of length `p` or `p` by `1` matrix.
#'   Standard errors of estimates of regression slopes.
#' @param slopes Numeric vector of length `p` or `p` by `1` matrix.
#'   The vector \eqn{\boldsymbol{\hat{\beta}_{2, \cdots, k}}} is a \eqn{p \times 1} vector of estimates
#'   of \eqn{p} unknown regression slopes.
#' @param slopesprime Numeric vector of length `p` or `p` by `1` matrix.
#'   The vector \eqn{\boldsymbol{\hat{\beta}_{2, \cdots, k}^{\prime}}} is a \eqn{p \times 1} vector of standardized estimates
#'   of \eqn{p} unknown regression slopes.
#' @export
.sehatslopesprimetb <- function(slopes = NULL,
                                sehatslopes = NULL,
                                slopesprime = NULL,
                                X,
                                y) {
  if (is.null(slopes)) {
    slopes <- slopes(
      X = X,
      y = y
    )
  }
  if (is.null(slopesprime)) {
    slopesprime <- slopesprime(
      X = X,
      y = y
    )
  }
  if (is.null(sehatslopes)) {
    sehatbetahat <- as.vector(
      sehatbetahat(
        X = X,
        y = y
      )
    )
    sehatslopes <- sehatbetahat[-1]
  }
  out <- as.vector(sehatslopes) * (as.vector(slopesprime) / as.vector(slopes))
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatslopesprime"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatslopesprimetb
#' @inherit .sehatslopesprimetb description
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatslopesprimetb(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatslopesprimetb(X = X, y = y)
#' @export
sehatslopesprimetb <- function(X,
                               y) {
  .sehatslopesprimetb(
    slopes = NULL,
    sehatslopes = NULL,
    slopesprime = NULL,
    X = X,
    y = y
  )
}

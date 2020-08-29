#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Estimates of Regression Coefficients (from \eqn{\hat{\sigma}_{\varepsilon \ \textrm{unbiased}}^{2}})
#'
#' @description \eqn{\widehat{\mathbf{se}}} is equal to the square root of the diagonal elements
#'   of \eqn{\widehat{\mathrm{cov}} \left( \boldsymbol{\hat{\beta}} \right)} .
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords se
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
#' @keywords se
#' @inheritParams .sehatbetahat
#' @inherit .sehatbetahat description
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
#' @keywords se
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
#' @keywords se
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
#' @details \deqn{
#'     \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}} =
#'     \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{2, \cdots, k}} \frac{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}}{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
#'   }
#'   According to Yuan and Chan (2011), this standard error is biased.
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords se
#' @inheritParams .Xbetahat
#' @inheritParams .intercepthat
#' @param slopeshatprime Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} column vector of estimated standardized regression slopes
#'   \eqn{\left( \boldsymbol{\hat{\beta}}_{2, 3, \cdots, k}  = \left\{ \hat{\beta}_2, \hat{\beta}_3, \cdots, \hat{\beta}_k \right\}^{T} \right)} .
#' @param sehatslopeshat Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} column vector of estimated standard errors of estimates of regression slopes
#'   \eqn{\left( \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{2, 3, \cdots, k}^{\prime}} = \left\{ \mathrm{\hat{se}}_{\hat{\beta}_{2}^{\prime}}, \mathrm{\hat{se}}_{\hat{\beta}_{3}^{\prime}}, \cdots, \mathrm{\hat{se}}_{\hat{\beta}_{k}^{\prime}} \right\}^{T} \right)} .
#' @references
#' Yuan, K., Chan, W. (2011).
#'   Biases and Standard Errors of Standardized Regression Coefficients.
#'   *Psychometrika* 76, 670-690.
#'   [doi:10.1007/s11336-011-9224-6](https://doi.org/10.1007/s11336-011-9224-6).
#' @export
.sehatslopeshatprimetb <- function(slopeshat = NULL,
                                   sehatslopeshat = NULL,
                                   slopeshatprime = NULL,
                                   X,
                                   y) {
  if (is.null(slopeshat)) {
    slopeshat <- slopeshat(
      X = X,
      y = y
    )
  }
  if (is.null(slopeshatprime)) {
    slopeshatprime <- slopeshatprime(
      X = X,
      y = y
    )
  }
  if (is.null(sehatslopeshat)) {
    sehatbetahat <- as.vector(
      sehatbetahat(
        X = X,
        y = y
      )
    )
    sehatslopeshat <- sehatbetahat[-1]
  }
  out <- as.vector(sehatslopeshat) * (as.vector(slopeshatprime) / as.vector(slopeshat))
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatslopeshatprime"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Textbook)
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords se
#' @inheritParams .sehatslopeshatprimetb
#' @inherit .sehatslopeshatprimetb description references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatslopeshatprimetb(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatslopeshatprimetb(X = X, y = y)
#' @export
sehatslopeshatprimetb <- function(X,
                                  y) {
  .sehatslopeshatprimetb(
    slopeshat = NULL,
    sehatslopeshat = NULL,
    slopeshatprime = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Yuan and Chan (2011))
#'
#' @details The \eqn{p}th estimated standard error is calculated using
#'   \deqn{
#'     \mathbf{\widehat{se}}_{\boldsymbol{\hat{\beta}}_{\text{p}}^{\prime}} =
#'     \sqrt{
#'       \frac{\hat{\sigma}_{X_{p}}^{2} \hat{c}_{p} \hat{\sigma}_{\hat{\varepsilon}}^{2}}{n \hat{\sigma}_{y}^{2}} +
#'     \frac{\hat{\beta}_{p}^{2}
#'       \left[
#'         \hat{\sigma}_{X_{p}}^{2}
#'         \left( \boldsymbol{\hat{\beta}}^{T} \boldsymbol{\hat{\Sigma}}_{X} \boldsymbol{\hat{\beta}} \right) -
#'         \hat{\sigma}_{X_{p}}^{2} \hat{\sigma}_{\hat{\varepsilon}}^{2} -
#'         \hat{\sigma}_{y, X_{p}}^{2}
#'       \right]}{n \hat{\sigma}_{y}^{4}}
#'     }
#'   }
#'   where
#'   - \eqn{p = \left\{2, 3, \cdots, k \right\}}
#'   - \eqn{\hat{\sigma}_{\hat{\varepsilon}}^{2}} is the estimated residual variance
#'   - \eqn{\boldsymbol{\hat{\beta}}_{2, 3, \cdots, k} = \left\{ \hat{\beta}_{2}, \hat{\beta}_{3}, \cdots, \hat{\beta}_{k}\right\}^{T}}
#'     is the \eqn{p \times 1} column vector of estimated regression slopes
#'   - \eqn{\hat{\sigma}_{y}^{2}} is the variance of the regressand variable \eqn{y}
#'   - \eqn{\boldsymbol{\hat{\Sigma}}_{\mathbf{X}}} is the \eqn{p \times p} estimated covariance matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   - \eqn{\hat{\sigma}_{X_p}^{2}} is the variance of the corresponding \eqn{p}th regressor variable.
#'   - \eqn{\hat{\sigma}_{y, X_{p}}^{2}} is the covariance of the regressand variable \eqn{y} and the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   - \eqn{c_p} is the diagonal element that corresponds to the regressor variable in \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}^{-1}}
#'   - \eqn{n} is the sample size
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords se
#' @inheritParams .sehatslopeshatprimetb
#' @inheritParams .vcovhatbetahat
#' @inheritParams .slopeshat
#' @inherit .sehatslopeshatprimetb references
#' @param sigma2yhat Numeric.
#'   Estimated variance of the regressand \eqn{\left( \hat{\sigma}_{y}^{2} \right)}
#' @param adjust Logical.
#'   Use \eqn{n - 3} adjustment for small samples.
#' @param n Integer.
#'   Sample size.
#' @examples
#' slopes <- c(-3.0748755, -1.5653133, 1.0959758, 1.3703010, 0.1666065)
#' SigmaXhat <- matrix(
#'   data = c(
#'     0.25018672, 0.00779108, -0.01626038, -0.04424864, -0.13217068,
#'     0.00779108, 0.12957466, 0.01061297, -0.08818286, -0.16427222,
#'     -0.016260378, 0.010612975, 0.133848763, 0.004083767, 0.658462191,
#'     -0.044248635, -0.088182856, 0.004083767, 7.917601877, -5.910469742,
#'     -0.1321707, -0.1642722, 0.6584622, -5.9104697, 136.0217584
#'   ),
#'   ncol = 5
#' )
#' sigma2hatepsilonhat <- 42.35584
#' sigma2yhat <- 62.35235
#' sigmayXhat <- c(-0.8819639, -0.3633559, 0.2953811, 10.1433433, 15.9481950)
#' n <- 1289
#' .sehatslopeshatprimedelta(
#'   slopes = slopes, sigma2hatepsilonhat = sigma2hatepsilonhat,
#'   SigmaXhat = SigmaXhat, sigma2yhat = sigma2yhat, sigmayXhat = sigmayXhat, n = n
#' )
#' @export
.sehatslopeshatprimedelta <- function(slopeshat,
                                      sigma2hatepsilonhat,
                                      SigmaXhat,
                                      sigmayXhat,
                                      sigma2yhat,
                                      adjust = FALSE,
                                      n,
                                      X,
                                      y) {
  if (is.null(sigma2hatepsilonhat)) {
    sigma2hatepsilonhat <- sigma2hatepsilonhat(
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  if (is.null(slopeshat) | is.null(SigmaXhat) | is.null(sigmayXhat) | is.null(sigma2yhat)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      plot = FALSE,
      moments = FALSE,
      cor = FALSE,
      mardia = FALSE
    )
    SigmaXhat <- descriptives[["SigmaXhat"]]
    sigmayXhat <- descriptives[["sigmayXhat"]]
    sigma2yhat <- descriptives[["sigma2yhat"]]
    slopeshat <- .slopeshat(
      SigmaXhat = SigmaXhat,
      sigmayXhat = sigmayXhat
    )
    n <- nrow(X)
  }
  if (adjust) {
    n <- n - 3
  }
  slopeshat <- as.vector(slopeshat)
  sigma2Xhat <- diag(SigmaXhat)
  diagSigmaXhatinverse <- diag(solve(SigmaXhat))
  term <- drop(
    t(as.matrix(slopeshat)) %*% SigmaXhat %*% as.matrix(slopeshat)
  )
  # out <- rep(x = NA, times = length(slopeshat))
  # for (i in seq_along(out)) {
  #  out[i] <- sqrt(
  #    (
  #      sigma2Xhat[i] * diagSigmaXhatinverse[i] * sigma2hatepsilonhat
  #    ) / (
  #      n * sigma2yhat
  #    ) + (
  #      slopeshat[i]^2 * (
  #        (sigma2Xhat[i] * term) - (sigma2Xhat[i] * sigma2hatepsilonhat) - sigmayXhat[i]^2
  #      )
  #    ) / (
  #      n * sigma2yhat^2
  #    )
  #  )
  # }
  out <- sqrt(
    (
      sigma2Xhat * diagSigmaXhatinverse * sigma2hatepsilonhat
    ) / (
      n * sigma2yhat
    ) + (
      slopeshat^2 * (
        (sigma2Xhat * drop(
          t(
            as.matrix(
              slopeshat
            )
          ) %*% SigmaXhat %*% as.matrix(
            slopeshat
          )
        )
        ) - (sigma2Xhat * sigma2hatepsilonhat) - sigmayXhat^2
      )
    ) / (
      n * sigma2yhat^2
    )
  )
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatslopeshatprime"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Yuan and Chan (2011))
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords se
#' @inheritParams .sehatslopeshatprimedelta
#' @inheritParams betahat
#' @inherit .sehatslopeshatprimedelta details references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatslopeshatprimedelta(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatslopeshatprimedelta(X = X, y = y)
#' @export
sehatslopeshatprimedelta <- function(X,
                                     y,
                                     adjust = FALSE) {
  .sehatslopeshatprimedelta(
    slopeshat = NULL,
    sigma2hatepsilonhat = NULL,
    SigmaXhat = NULL,
    sigmayXhat = NULL,
    sigma2yhat = NULL,
    X = X,
    y = y,
    adjust = adjust
  )
}

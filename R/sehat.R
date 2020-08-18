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
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Textbook)
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Yuan and Chan (2011))
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatslopesprimetb
#' @inheritParams .vcovhatbetahat
#' @inheritParams betahat
#' @param SigmaXhat `p` by `p` numeric matrix.
#'   Estimated \eqn{p \times p} variances and covariances of \eqn{{X}_{2}, {X}_{3}, \cdots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\hat{\Sigma}}_{\mathbf{X}} \right)}.
#' @param sigma2yhat Numeric.
#'   Estimated variance of the regressand \eqn{\left( \hat{\sigma}_{y} \right)}
#' @param sigmayXhat Numeric vector of length `p` or `p` by `1` matrix.
#'   Estimated \eqn{p \times 1} covariances between the regressand and the regressors
#'   \eqn{\left( \boldsymbol{\sigma}_{\mathbf{yX}} \right)}.
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
#' .sehatslopesprimedelta(
#'   slopes = slopes, sigma2hatepsilonhat = sigma2hatepsilonhat,
#'   SigmaXhat = SigmaXhat, sigma2yhat = sigma2yhat, sigmayXhat = sigmayXhat, n = n
#' )
#' @export
.sehatslopesprimedelta <- function(slopes,
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
  if (is.null(slopes) | is.null(SigmaXhat) | is.null(sigmayXhat) | is.null(sigma2yhat)) {
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
    slopes <- as.vector(
      .slopes(
        SigmaX = SigmaXhat,
        sigmayX = sigmayXhat
      )
    )
    n <- nrow(X)
  }
  if (adjust) {
    n <- n - 3
  }
  slopes <- as.vector(slopes)
  sigma2Xhat <- diag(SigmaXhat)
  diagSigmaXhatinverse <- diag(solve(SigmaXhat))
  term <- drop(
    t(slopes) %*% SigmaXhat %*% slopes
  )
  out <- rep(x = NA, times = length(slopes))
  for (i in seq_along(out)) {
    out[i] <- sqrt(
      (
        sigma2Xhat[i] * diagSigmaXhatinverse[i] * sigma2hatepsilonhat
      ) / (
        n * sigma2yhat
      ) + (
        slopes[i]^2 * (
          (sigma2Xhat[i] * term) - (sigma2Xhat[i] * sigma2hatepsilonhat) - sigmayXhat[i]^2
        )
      ) / (
        n * sigma2yhat^2
      )
    )
  }
  out <- matrix(
    data = out,
    ncol = 1
  )
  colnames(out) <- "sehatslopesprime"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standard Errors of Standardized Estimates of Regression Coefficients (Yuan and Chan (2011))
#'
#' @family standard errors of estimates of regression coefficients functions
#' @keywords inference
#' @inheritParams .sehatslopesprimedelta
#' @inheritParams betahat
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' sehatslopesprimedelta(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' sehatslopesprimedelta(X = X, y = y)
#' @export
sehatslopesprimedelta <- function(X,
                                  y,
                                  adjust = FALSE) {
  .sehatslopesprimedelta(
    slopes = NULL,
    sigma2hatepsilonhat = NULL,
    SigmaXhat = NULL,
    sigmayXhat = NULL,
    sigma2yhat = NULL,
    X = X,
    y = y,
    adjust = adjust
  )
}

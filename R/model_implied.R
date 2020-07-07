#' Model-Implied Variance-Covariance Matrix
#' (\eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)})
#'
#' Model-implied variance-covariance matrix
#' (\eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)})
#' from parameters of a \eqn{k}-variable linear regression model.
#' The parameters of a linear regression model
#' for the covariance structure
#' are
#' the slopes
#' (\eqn{\boldsymbol{\beta}_{\mathrm{slopes}}}),
#' the variance of the error term \eqn{\boldsymbol{\varepsilon}}
#' (\eqn{\sigma^2}),
#' and
#' the variances and covariances of
#'   \eqn{
#'     {X}_{2},
#'     {X}_{3},
#'     \dots,
#'     {X}_{k}
#'   }
#' (\eqn{\boldsymbol{\Sigma}_{\mathbf{X}}}).
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param beta_slopes Vector or `p` by `1` matrix.
#'   \eqn{\left( p \right) \times 1}
#'   vector of regression slopes.
#' @param sigma2 Numeric.
#'   Variance of the error term
#'   \eqn{\boldsymbol{\varepsilon}}
#'   (\eqn{\sigma^2}).
#' @param SigmaX Matrix.
#'   Variances and covariances of
#'   \eqn{
#'     {X}_{2},
#'     {X}_{3},
#'     \dots,
#'     {X}_{k}
#'   }
#'   (\eqn{\boldsymbol{\Sigma}_{\mathbf{X}}}).
#' @return
#'   Returns the model-implied
#'   variance-covariance matrix
#'   (\eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}).
#' @export
Sigmatheta <- function(beta_slopes,
                       sigma2,
                       SigmaX) {
  ram_Sigmatheta <- function(A,
                             S,
                             F,
                             I) {
    x <- solve(I - A)
    F %*% x %*% S %*% t(x) %*% t(F)
  }
  beta_slopes <- as.vector(beta_slopes)
  p <- length(beta_slopes)
  k <- p + 1
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    beta_slopes
  )
  I <- F <- diag(k)
  SigmaX[upper.tri(SigmaX)] <- 0
  S <- SigmaX
  S <- cbind(
    0,
    S
  )
  S <- rbind(
    0,
    S
  )
  S[1, 1] <- sigma2
  ram_Sigmatheta(
    A = A,
    S = S,
    F = F,
    I = I
  )
}

#' Model-Implied Mean Vector
#' (\eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)})
#'
#' Model-implied mean vector
#' (\eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)})
#' from parameters of a \eqn{k}-variable linear regression model.
#' The parameters of a linear regression model
#' for the mean structure
#' are
#' the regression coefficients
#' (\eqn{\boldsymbol{\beta}}),
#' and
#' the means of
#' the regressors
#'   \eqn{
#'     {X}_{2},
#'     {X}_{3},
#'     \dots,
#'     {X}_{k}
#'   }
#' excluding the mean for the vector of constants
#' (\eqn{\boldsymbol{\mu}_{\mathbf{X}}}).
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param beta Vector or `k` by `1` matrix.
#'   The vector
#'   \eqn{\boldsymbol{\beta}}
#'   is a \eqn{k \times 1} vector
#'   of \eqn{k} regression coefficients.
#' @param muX Vector or `p` by `1` matrix.
#'   \eqn{\left( p \right) \times 1} vector
#'   of means of
#'   the regressors
#'     \eqn{
#'       {X}_{2},
#'       {X}_{3},
#'       \dots,
#'       {X}_{k}
#'     }
#'   excluding the mean for the vector of constants
#'   (\eqn{\boldsymbol{\mu}_{\mathbf{X}}}).
#' @export
mutheta <- function(beta,
                    muX) {
  ram_mutheta <- function(A,
                          F,
                          I,
                          M) {
    F %*% solve(I - A) %*% M
  }
  beta <- as.vector(beta)
  k <- length(beta)
  beta_intercept <- beta[1]
  beta_slopes <- beta[-1]
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    beta_slopes
  )
  M <- c(
    beta_intercept,
    as.vector(muX)
  )
  I <- F <- diag(k)
  ram_mutheta(
    A = A,
    F = F,
    I = I,
    M = M
  )
}

#' Mean Structure
#' (\eqn{\mathbf{M}})
#'
#' Mean structure column vector
#' (\eqn{\mathbf{M}})
#' from regression slopes
#' and means of observed variables
#' of a \eqn{k}-variable Linear Regression Model.
#'
#' The parameters of a linear regression model
#' for the mean structure
#' are
#' the slopes
#' (\eqn{\boldsymbol{\beta}_{\mathrm{slopes}}}),
#' the mean of
#' the regressand
#' \eqn{\mu_\mathbf{y}}
#' and
#' the means of
#' the regressors
#'   \eqn{
#'     {X}_{2},
#'     {X}_{3},
#'     \dots,
#'     {X}_{k}
#'   }
#' excluding the mean for the vector of constants
#' (\eqn{\boldsymbol{\mu}_{\mathbf{X}}}).
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams Sigmatheta
#' @inheritParams mutheta
#' @param muy Numeric.
#'   Mean of the regressand variable
#'   \eqn{\mu_{\mathbf{y}}}.
#' @export
mean_str <- function(beta_slopes,
                     muy,
                     muX) {
  ram_M <- function(A,
                    F,
                    I,
                    mu) {
    F %*% (I - A) %*% mu
  }
  beta_slopes <- as.vector(beta_slopes)
  p <- length(beta_slopes)
  k <- p + 1
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    beta_slopes
  )
  I <- F <- diag(k)
  mu <- c(
    muy,
    as.vector(muX)
  )
  ram_M(
    A = A,
    F = F,
    I = I,
    mu = mu
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Variance-Covariance Matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}
#'
#' @description Model-implied variance-covariance matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}
#'   from parameters of a \eqn{k}-variable linear regression model.
#'
#' @details The following are the parameters of a linear regression model
#'   for the covariance structure
#'   - \eqn{\boldsymbol{\beta}_{\mathrm{slopes}}} - \eqn{p \times 1} regression slopes,
#'   - \eqn{\sigma^2} - variance of the error term
#'     \eqn{\varepsilon}, and
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}} - \eqn{p \times p}
#'     variances and covariances of \eqn{{X}_{2}, {X}_{3}, \dots, {X}_{k}}.
#'
#'   The model-implied variance-covariance matrix is given by
#'   \deqn{
#'     \boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)
#'     =
#'     \left( \mathrm{I} - \mathrm{A} \right)^{-1}
#'     \mathrm{S}
#'     \left[ \left( \mathrm{I} - \mathrm{A} \right)^{-1} \right]^{\prime}
#'   }
#'   where
#'   \deqn{
#'     \mathbf{A}
#'     =
#'     \begin{bmatrix}
#'       0 & \beta_2 & \beta_3 & \cdots & \beta_k \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0
#'     \end{bmatrix}
#'   }
#'
#'   \deqn{
#'     \mathbf{S}
#'     =
#'     \begin{bmatrix}
#'       \sigma^2 & 0                         & 0                         & \cdots & 0                         \\
#'       0        & \mathrm{Var}_{X_2}        & \mathrm{Cov}_{X_{2}X_{3}} & \cdots & \mathrm{Cov}_{X_{2}X_{k}} \\
#'       0        & \mathrm{Cov}_{X_{3}X_{2}} & \mathrm{Var}_{X_3}        & \cdots & \mathrm{Cov}_{X_{3}X_{k}} \\
#'       0        & \vdots                    & \vdots                    & \ddots & \vdots                    \\
#'       0        & \mathrm{Cov}_{X_{k}X_{2}} & \mathrm{Cov}_{X_{k}X_{3}} & \cdots & \mathrm{Var}_{X_k}
#'     \end{bmatrix}
#'   }
#'
#'   \deqn{
#'     \mathbf{I}
#'     =
#'     \begin{bmatrix}
#'       1 & 0   & 0   & \cdots & 0   \\
#'       0 & 1   & 0   & \cdots & 0   \\
#'       0 & 0   & 1   & \cdots & 0   \\
#'       0 & 0   & 0   & 1      & 0   \\
#'       0 & 0   & 0   & \cdots & 1
#'     \end{bmatrix}
#'   }
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param slopes Vector or `p` by `1` matrix.
#'   Regression slopes \eqn{\left( \boldsymbol{\beta}_{2 \cdots k} \right)} .
#' @param sigma2 Numeric.
#'   Variance of the error term \eqn{\varepsilon} \eqn{\left( \sigma^2 \right)}.
#' @param SigmaX `p` by `p` numeric matrix.
#'   Variances and covariances of \eqn{{X}_{2}, {X}_{3}, \dots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\Sigma}_{\mathbf{X}} \right)}.
#' @return Returns the model-implied variance-covariance matrix
#'   \eqn{\boldsymbol{\Sigma} \left( \boldsymbol{\theta} \right)}.
#' @export
Sigmatheta <- function(slopes,
                       sigma2,
                       SigmaX) {
  foo <- function(A,
                  S) {
    invIminusA <- solve(diag(nrow(A)) - A)
    invIminusA %*% S %*% t(invIminusA)
  }
  slopes <- as.vector(slopes)
  p <- length(slopes)
  k <- p + 1
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    slopes
  )
  filter <- diag(k)
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
  foo(
    A = A,
    S = S
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Mean Vector
#'   \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}
#'
#' @description Model-implied mean vector
#' \eqn{\boldsymbol{\mu} \left( \boldsymbol{\theta} \right)}
#' from parameters of a \eqn{k}-variable linear regression model.
#'
#' @details The following are the parameters of a linear regression model
#'   for the mean structure
#'   - \eqn{\boldsymbol{\beta}} - \eqn{k \times k} regression coefficients and
#'   - \eqn{\boldsymbol{\mu}_{\mathbf{X}}} - \eqn{p \times 1} vector of means of the regressors
#'     \eqn{ {X}_{2}, {X}_{3}, \dots, {X}_{k}} .
#'
#'   The model-implied mean vector is given by
#'   \deqn{
#'     \boldsymbol{\mu} \left( \boldsymbol{\theta} \right)
#'     =
#'     \left( \mathrm{I} - \mathrm{A} \right)^{-1}
#'     \mathrm{M}
#'   }
#'   where
#'   \deqn{
#'     \mathbf{A}
#'     =
#'     \begin{bmatrix}
#'       0 & \beta_2 & \beta_3 & \cdots & \beta_k \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0       \\
#'       0 & 0       & 0       & \cdots & 0
#'     \end{bmatrix}
#'   }
#'
#'   \deqn{
#'     \mathbf{M}
#'     =
#'     \begin{bmatrix}
#'       \beta_1     \\
#'       \mu_{X_{2}} \\
#'       \mu_{X_{3}} \\
#'       \vdots      \\
#'       \mu_{X_{k}}
#'     \end{bmatrix}
#'   }
#'
#' @family model-implied functions
#' @keywords model-implied
#' @param beta Vector or `k` by `1` matrix.
#'   \eqn{k \times 1} regression coefficients \eqn{\left( \boldsymbol{\beta} \right)}.
#' @param muX Vector or `p` by `1` matrix. \eqn{p \times 1} vector
#'   of means of the regressors \eqn{ {X}_{2}, {X}_{3}, \dots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
#' @export
mutheta <- function(beta,
                    muX) {
  foo <- function(M,
                  A) {
    solve(diag(nrow(A)) - A) %*% M
  }
  beta <- as.vector(beta)
  k <- length(beta)
  intercept <- beta[1]
  slopes <- beta[-1]
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    slopes
  )
  M <- c(
    intercept,
    as.vector(muX)
  )
  foo(
    M = M,
    A = A
  )
}

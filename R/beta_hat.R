#' Beta-hat - Inverse of X
#'
#' Estimates coefficients of a linear regression model
#'   using
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{X}
#'     \right)^{-1}
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{y}
#'     \right).
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param X Matrix.
#'   The data matrix
#'   \eqn{\mathbf{X}}
#'   is an \eqn{n \times k} matrix
#'   of \eqn{n} observations
#'   of \eqn{k} regressors,
#'   which includes a regressor
#'   whose value is 1 for each observation.
#' @param y Vector or `n` by `1` matrix.
#'   The vector
#'   \eqn{\mathbf{y}}
#'   is an \eqn{n \times 1} vector
#'   of observations on the regressand variable.
#' @return
#'   Returns \eqn{\boldsymbol{\hat{\beta}}}
#'   that is,
#'   a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients
#'   estimated using ordinary least squares.
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: Inverting the matrix of the normal equations](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Inverting_the_matrix_of_the_normal_equations)
#' @family beta-hat functions
#' @export
beta_hat_inv <- function(X,
                         y) {
  drop(
    solve(
      crossprod(X),
      crossprod(X, y)
    )
  )
}

#' Beta-hat - QR Decomposition
#'
#' Estimates coefficients of a linear regression model
#'   using QR Decomposition.
#'   The data matrix \eqn{\mathbf{X}} is decomposed into
#'   \deqn{
#'     \mathbf{X}
#'     =
#'     \mathbf{Q}
#'     \mathbf{R}.
#'   }
#'   Estimates are found by solving
#'   \deqn{
#'     \mathbf{R}
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \mathbf{Q}^{\prime}
#'     \mathbf{y}.
#'   }
#'
#' @inheritParams beta_hat_inv
#' @inherit beta_hat_inv return
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: QR Decomposition](https://en.wikipedia.org/wiki/QR_decomposition)
#'
#'   [Wikipedia: Orthogonal decomposition methods](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Orthogonal_decomposition_methods)
#' @family beta-hat functions
#' @export
beta_hat_qr <- function(X,
                        y) {
  Xqr <- qr(X)
  drop(
    backsolve(
      qr.R(Xqr),
      crossprod(qr.Q(Xqr), y)
    )
  )
}

#' Beta-hat - Singular Value Decomposition
#'
#' Estimates coefficients of a linear regression model
#'   using Singular Value Decomposition.
#'   The data matrix \eqn{\mathbf{X}} is decomposed into
#'   \deqn{
#'     \mathbf{X}
#'     =
#'     \mathbf{U}
#'     \mathbf{\Sigma}
#'     \mathbf{V}^{\prime}.
#'   }
#'   Estimates are found by solving
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \mathbf{V}
#'     \mathbf{\Sigma}^{+}
#'     \mathbf{U}^{\prime}
#'     \mathbf{y}
#'   }
#'   where the superscript \eqn{+} indicates the pseudoinverse.
#'
#' @inheritParams beta_hat_inv
#' @inherit beta_hat_inv return
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: Singular Value Decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)
#'
#'   [Wikipedia: Orthogonal decomposition methods](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Orthogonal_decomposition_methods)
#' @family beta-hat functions
#' @export
beta_hat_svd <- function(X,
                         y) {
  Xsvd <- svd(X)
  drop(
    (Xsvd$v %*% (1 / Xsvd$d * t(Xsvd$u))) %*% y
  )
}

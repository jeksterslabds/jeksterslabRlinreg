#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients
#'   \eqn{
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \left( \mathbf{X}^{T} \mathbf{y} \right)
#'   }
#'
#' @description Estimates coefficients of a linear regression model using
#'   \deqn{\boldsymbol{\hat{\beta}}
#'     =
#'     \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \left( \mathbf{X}^{T} \mathbf{y} \right) .
#'   }
#'
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: Inverting the matrix of the normal equations](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Inverting_the_matrix_of_the_normal_equations)
#'
#'   [Wikipedia: Design Matrix](https://en.wikipedia.org/wiki/Design_matrix)
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @importFrom jeksterslabRmatrix is.singular
#' @param X Matrix.
#'   The data matrix \eqn{\mathbf{X}}
#'   (also known as design matrix, model matrix or regressor matrix)
#'   is an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
#'   which includes a regressor whose value is 1 for each observation.
#' @param y Vector or `n` by `1` matrix.
#'   The vector \eqn{\mathbf{y}} is an \eqn{n \times 1} vector of observations
#'   on the regressand variable.
#' @return Returns \eqn{\boldsymbol{\hat{\beta}}}, that is,
#'   a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients
#'   estimated using ordinary least squares.
#' @export
betahatinv <- function(X,
                       y) {
  XTX <- crossprod(X)
  if (is.singular(XTX)) {
    stop(
      "X transpose X is singular."
    )
  }
  drop(
    solve(
      XTX,
      crossprod(X, y)
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients
#'   \eqn{\boldsymbol{\hat{\beta}}} - QR Decomposition
#'
#' @description Estimates coefficients of a linear regression model
#'   using QR Decomposition.
#'   The data matrix \eqn{\mathbf{X}} is decomposed into
#'   \deqn{
#'     \mathbf{X}
#'     =
#'     \mathbf{Q}
#'     \mathbf{R}.
#'   }
#'   Estimates are found by solving \eqn{\boldsymbol{\hat{\beta}}} in
#'   \deqn{
#'     \mathbf{R}
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \mathbf{Q}^{T}
#'     \mathbf{y}.
#'   }
#'
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: QR Decomposition](https://en.wikipedia.org/wiki/QR_decomposition)
#'
#'   [Wikipedia: Orthogonal decomposition methods](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Orthogonal_decomposition_methods)
#'
#'   [Wikipedia: Design Matrix](https://en.wikipedia.org/wiki/Design_matrix)
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahatinv
#' @inherit betahatinv return
#' @export
betahatqr <- function(X,
                      y) {
  Xqr <- qr(X)
  drop(
    backsolve(
      qr.R(Xqr),
      crossprod(
        qr.Q(Xqr),
        y
      )
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients
#'   \eqn{\boldsymbol{\hat{\beta}}} - Singular Value Decomposition
#'
#' @description Estimates coefficients of a linear regression model
#'   using Singular Value Decomposition.
#'   The data matrix \eqn{\mathbf{X}} is decomposed into
#'   \deqn{
#'     \mathbf{X}
#'     =
#'     \mathbf{U}
#'     \mathbf{\Sigma}
#'     \mathbf{V}^{T}.
#'   }
#'   Estimates are found by solving
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}
#'     =
#'     \mathbf{V}
#'     \mathbf{\Sigma}^{+}
#'     \mathbf{U}^{T}
#'     \mathbf{y}
#'   }
#'   where the superscript \eqn{+} indicates the pseudoinverse.
#'
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: Singular Value Decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)
#'
#'   [Wikipedia: Orthogonal decomposition methods](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Orthogonal_decomposition_methods)
#'
#'   [Wikipedia: Design Matrix](https://en.wikipedia.org/wiki/Design_matrix)
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahatinv
#' @inherit betahatinv return
#' @export
betahatsvd <- function(X,
                       y) {
  Xsvd <- svd(X)
  drop(
    Xsvd$v %*% ((1 / Xsvd$d) * crossprod(Xsvd$u, y))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients \eqn{\boldsymbol{\hat{\beta}}}
#'
#' @description Estimates coefficients of a linear regression model.
#'
#' @references
#'   [Wikipedia: Linear Regression](https://en.wikipedia.org/wiki/Linear_regression)
#'
#'   [Wikipedia: Ordinary Least Squares](https://en.wikipedia.org/wiki/Ordinary_least_squares)
#'
#'   [Wikipedia: Inverting the matrix of the normal equations](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Inverting_the_matrix_of_the_normal_equations)
#'
#'   [Wikipedia: QR Decomposition](https://en.wikipedia.org/wiki/QR_decomposition)
#'
#'   [Wikipedia: Singular Value Decomposition](https://en.wikipedia.org/wiki/Singular_value_decomposition)
#'
#'   [Wikipedia: Orthogonal decomposition methods](https://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares#Orthogonal_decomposition_methods)
#'
#'   [Wikipedia: Design Matrix](https://en.wikipedia.org/wiki/Design_matrix)
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahatinv
#' @inherit betahatinv return
#' @param qr Logical.
#'   If `TRUE`, use QR decomposition when normal equations fail.
#'   If `FALSE`, use singular value decompositon when normal equations fail.
#' @export
betahat <- function(X,
                    y,
                    qr = TRUE) {
  tryCatch(
    {
      out <- betahatinv(
        X = X,
        y = y
      )
      return(out)
    },
    error = function(e) {
      if (qr) {
        message(
          "Using QR decomposition."
        )
        out <- betahatqr(
          X = X,
          y = y
        )
        return(out)
      } else {
        message(
          "Using singular value decomposition."
        )
        out <- betahatqr(
          X = X,
          y = y
        )
        return(out)
      }
    }
  )
}

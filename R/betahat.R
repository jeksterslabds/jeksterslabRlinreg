#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients
#'   \eqn{
#'     \boldsymbol{\hat{\beta}} = \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \left( \mathbf{X}^{T} \mathbf{y} \right)
#'   }
#'
#' @description Estimates coefficients of a linear regression model using
#'   \deqn{
#'     \boldsymbol{\hat{\beta}} = \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \left( \mathbf{X}^{T} \mathbf{y} \right) .
#'   }
#'   Also know as the normal equation.
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
#' @param X `n` by `k` numeric matrix.
#'   The data matrix \eqn{\mathbf{X}}
#'   (also known as design matrix, model matrix or regressor matrix)
#'   is an \eqn{n \times k} matrix of \eqn{n} observations of \eqn{k} regressors,
#'   which includes a regressor whose value is 1 for each observation on the first column.
#' @param y Numeric vector of length `n` or `n` by `1` matrix.
#'   The vector \eqn{\mathbf{y}} is an \eqn{n \times 1} vector of observations
#'   on the regressand variable.
#' @return Returns \eqn{\boldsymbol{\hat{\beta}}}, that is,
#'   a \eqn{k \times 1} vector of estimates
#'   of \eqn{k} unknown regression coefficients
#'   estimated using ordinary least squares.
#' @export
.betahatnorm <- function(X,
                         y) {
  drop(
    solve(
      crossprod(X),
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
#'     \mathbf{X} = \mathbf{Q} \mathbf{R} .
#'   }
#'   Estimates are found by solving \eqn{\boldsymbol{\hat{\beta}}} in
#'   \deqn{
#'     \mathbf{R} \boldsymbol{\hat{\beta}} = \mathbf{Q}^{T} \mathbf{y}.
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
#' @inheritParams .betahatnorm
#' @inherit .betahatnorm return
#' @export
.betahatqr <- function(X,
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
#'     \mathbf{X} = \mathbf{U} \mathbf{\Sigma} \mathbf{V}^{T} .
#'   }
#'   Estimates are found by solving
#'   \deqn{
#'     \boldsymbol{\hat{\beta}} =
#'     \mathbf{V} \mathbf{\Sigma}^{+} \mathbf{U}^{T} \mathbf{y}
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
#' @inheritParams .betahatnorm
#' @inherit .betahatnorm return
#' @export
.betahatsvd <- function(X,
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
#' @details Calculates coefficients using the normal equation.
#'   When that fails, QR decomposition is used when `qr = TRUE`
#'   or singular value decomposition when `qr = FALSE`.
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
#' @inheritParams .betahatnorm
#' @inherit .betahatnorm return
#' @param qr Logical.
#'   If `TRUE`, use QR decomposition when normal equations fail.
#'   If `FALSE`, use singular value decompositon when normal equations fail.
#' @export
betahat <- function(X,
                    y,
                    qr = TRUE) {
  tryCatch(
    {
      out <- .betahatnorm(
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
        out <- .betahatqr(
          X = X,
          y = y
        )
        return(out)
      } else {
        message(
          "Using singular value decomposition."
        )
        out <- .betahatsvd(
          X = X,
          y = y
        )
        return(out)
      }
    }
  )
}

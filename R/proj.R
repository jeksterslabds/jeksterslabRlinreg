#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title P Matrix
#'
#' @description Calculates the projection matrix \eqn{\left( \mathbf{P} \right)}
#'   using
#'   \deqn{
#'     \mathbf{P} = \mathbf{X} \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1}
#'     \mathbf{X}^{T} .
#'   }
#'
#' @details The projection matrix \eqn{\left( \mathbf{P} \right)},
#'   also known as the hat matrix, transforms the \eqn{\mathbf{y}} vector
#'   to the vector of predicted values
#'   \eqn{\left( \mathbf{\hat{y}} = \mathbf{Py} \right)}.
#'
#' @family projection matrix functions
#' @keywords projection
#' @inheritParams betahat
#' @return Returns the projection matrix \eqn{\left( \mathbf{P} \right)}.
#' @references
#'   [Wikipedia: Projection Matrix](https://en.wikipedia.org/wiki/Projection_matrix)
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' P <- P(X = X)
#' str(P, list.len = 6)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' P <- P(X = X)
#' str(P, list.len = 6)
#' @export
P <- function(X) {
  X %*% solve(t(X) %*% X) %*% t(X)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title M Matrix
#'
#' @description Calculates the residual maker matrix
#'   \eqn{\left( \mathbf{M} \right)} using
#'   \deqn{
#'     \mathbf{M} = \mathbf{I} - \mathbf{P}
#'   }
#'   where
#'   \deqn{
#'     \mathbf{P} = \mathbf{X} \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1} \mathbf{X}^{T} .
#'   }
#'   The residual maker matrix \eqn{\left( \mathbf{M} \right)}
#'   transforms the \eqn{\mathbf{y}} vector
#'   to the vector of residuals \eqn{\left( \mathbf{e} = \mathbf{My} \right)} .
#'
#' @details If `P = NULL`, the `P` matrix is computed using [`P()`].
#'
#' @family projection matrix functions
#' @keywords projection
#' @inheritParams betahat
#' @inherit P references
#' @inherit P references
#' @param P `n` by `n` numeric matrix.
#'   The \eqn{n \times n} projection matrix \eqn{\left( \mathbf{P} \right)}.
#' @return Returns the residual maker matrix \eqn{\left( \mathbf{M} \right)} .
#' @export
.M <- function(X,
               P = NULL) {
  if (is.null(P)) {
    P <- P(X)
  }
  diag(nrow(X)) - P
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title M Matrix
#'
#' @family projection matrix functions
#' @keywords projection
#' @inheritParams .M
#' @inherit .M description return references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' M <- M(X = X)
#' str(M, list.len = 6)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' M <- M(X = X)
#' str(M, list.len = 6)
#' @export
M <- function(X) {
  .M(
    X = X,
    P = NULL
  )
}

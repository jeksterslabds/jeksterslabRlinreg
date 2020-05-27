#' P Matrix
#'
#' Calculates the projection matrix (\eqn{\mathbf{P}})
#'   using
#'   \deqn{
#'     \mathbf{P}
#'     =
#'     \mathbf{X}
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{X}
#'     \right)^{-1}
#'     \mathbf{X}^{\prime}.
#'   }
#'
#' The projection matrix
#' (\eqn{\mathbf{P}}),
#' also known as the hat matrix,
#' transforms the \eqn{\mathbf{y}} vector
#' to the vector of predicted values
#' (\eqn{\mathbf{\hat{y}} = \mathbf{Py}}).
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams betahat_inv
#' @return Returns the projection matrix (\eqn{\mathbf{P}}).
#' @references
#'   [Wikipedia: Projection Matrix](https://en.wikipedia.org/wiki/Projection_matrix)
#' @family projection matrix functions
#' @export
P <- function(X) {
  X %*% solve(t(X) %*% X) %*% t(X)
}

#' M Matrix
#'
#' Calculates the residual maker matrix (\eqn{\mathbf{M}})
#'   using
#'   \deqn{
#'     \mathbf{M}
#'     =
#'     \mathbf{I} - \mathbf{X}
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{X}
#'     \right)^{-1}
#'     \mathbf{X}^{\prime}
#'     =
#'     \mathbf{I} - \mathbf{P}.
#'   }
#'
#' If `P = NULL`,
#' the `P` matrix is computed
#' using [`P()`].
#' The residual maker matrix (\eqn{\mathbf{M}})
#' transforms the \eqn{\mathbf{y}} vector
#' to the vector of residuals
#' (\eqn{\mathbf{e} = \mathbf{My}}).
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param P Matrix.
#'   The projection matrix
#'   (\eqn{\mathbf{P}}).
#' @inheritParams betahat_inv
#' @return Returns the residual maker matrix (\eqn{\mathbf{M}}).
#' @inherit P references
#' @family projection matrix functions
#' @export
M <- function(X,
              P = NULL) {
  if (is.null(P)) {
    P <- P(X)
  }
  diag(nrow(X)) - P
}

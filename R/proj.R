#' P Matrix
#'
#' Calculates the projection matrix
#' \eqn{\left( \mathbf{P} \right)}
#' using
#' \deqn{
#'   \mathbf{P}
#'   =
#'   \mathbf{X}
#'   \left(
#'     \mathbf{X}^{\prime}
#'     \mathbf{X}
#'   \right)^{-1}
#'   \mathbf{X}^{\prime}.
#' }
#'
#' The projection matrix
#' \eqn{\left( \mathbf{P} \right)},
#' also known as the hat matrix,
#' transforms the \eqn{\mathbf{y}} vector
#' to the vector of predicted values
#' \eqn{\left( \mathbf{\hat{y}} = \mathbf{Py} \right)}.
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
#'     \mathbf{X}^{\prime} \\
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

#' Leverage
#'
#' Calculates leverage,
#' that is,
#' how far away the regressor values
#' of an observation are
#' from those of the other observations using
#'   \deqn{
#'     h_{ii}
#'     =
#'     x_{i}^{\prime}
#'     \left(
#'       \mathbf{X}^{\prime}
#'       \mathbf{X}
#'     \right)^{-1}
#'     x_{i}
#'   }
#' where \eqn{x_{i}^{\prime}}
#' is the \eqn{i}th row of the \eqn{\mathbf{X}} matrix.
#' Note that
#' \eqn{\mathbf{X} \left( \mathbf{X}^{\prime} \mathbf{X} \right)^{-1} \mathbf{X}^{\prime}}
#' is the projection matrix (or hat matrix) \eqn{\mathbf{P}}
#' and \eqn{h_{ii}} is the diagonal of \eqn{\mathbf{P}}.
#'
#' If `P = NULL`,
#' `P` is computed with
#' `X` as a required argument.
#' `X` is ignored if `P` is provided.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param P Matrix
#'   The projection matrix.
#' @inheritParams P
#' @family projection matrix functions
#' @export
.h <- function(P = NULL,
               X = NULL) {
  if (is.null(P)) {
    P <- P(X)
  }
  diag(P)
}

#' Leverage
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .h
#' @inherit .h description
#' @family projection matrix functions
#' @export
h <- function(X) {
  .h(
    P = NULL,
    X = X
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Leverage
#'
#' @description Calculates leverage, that is, how far away the regressor values
#'   of an observation are from those of the other observations using
#'   \deqn{
#'     h_{ii} = x_{i}^{T} \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1} x_{i}
#'   }
#'   where \eqn{x_{i}^{T}}
#'   is the \eqn{i}th row of the \eqn{\mathbf{X}} matrix.
#'   Note that
#'   \eqn{
#'     \mathbf{X} \left( \mathbf{X}^{T} \mathbf{X} \right)^{-1} \mathbf{X}^{T}
#'   }
#'   is the projection matrix (or hat matrix) \eqn{\mathbf{P}}
#'   and \eqn{h_{ii}} is the diagonal of \eqn{\mathbf{P}}.
#'
#' @details If `P = NULL`, `P` is computed with `X` as a required argument.
#'   `X` is ignored if `P` is provided.
#'
#' @family projection matrix functions
#' @keywords projection
#' @inheritParams P
#' @param P Numeric matrix
#'   The projection matrix.
#' @references
#' [Wikipedia: Leverage](https://en.wikipedia.org/wiki/Leverage_(statistics))
#' @return Returns leverage.
#' @export
.h <- function(P = NULL,
               X = NULL) {
  if (is.null(P)) {
    P <- P(X)
  }
  diag(P)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Leverage
#'
#' @family projection matrix functions
#' @keywords projection
#' @inheritParams .h
#' @inherit .h description references return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' h <- h(
#'   X = X
#' )
#' hist(h)
#' @export
h <- function(X) {
  .h(
    P = NULL,
    X = X
  )
}

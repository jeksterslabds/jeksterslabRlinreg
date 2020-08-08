#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Plots
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams betahat
#' @importFrom jeksterslabRplots .residual.plot
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' residual.plot(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' residual.plot(X = X, y = y)
#' @export
residual.plot <- function(X,
                          y) {
  .residual.plot(
    yhat = yhat(
      X = X,
      y = y
    ),
    epsilonhat = epsilonhat(
      X = X,
      y = y
    ),
    tepsilonhat = tepsilonhat(
      X = X,
      y = y
    ),
    h = h(
      X = X
    )
  )
}

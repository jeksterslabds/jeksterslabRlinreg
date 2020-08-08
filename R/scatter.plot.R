#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Scatter Plot Matrix
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams betahat
#' @importFrom jeksterslabRplots .scatter.plot
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' scatter.plot(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' scatter.plot(X = X, y = y)
#' @export
scatter.plot <- function(X,
                         y) {
  data <- cbind(
    y,
    X[, -1]
  )
  .scatter.plot(data = data)
}

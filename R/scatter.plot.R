#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Scatter Plot Matrix
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams betahat
#' @importFrom jeksterslabRplots .scatter.plot
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' scatter.plot(X = X, y = y)
#' @export
scatter.plot <- function(X,
                         y) {
  data <- cbind(
    y,
    X
  )
  .scatter.plot(data = data)
}

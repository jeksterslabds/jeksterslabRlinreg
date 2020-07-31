#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model Assessment
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .R2fromRSS
#' @inheritParams .Rbar2
#' @return Returns a vector with the following elements
#' \describe{
#'   \item{RSS}{Residual sum of squares.}
#'   \item{MSE}{Mean square error.}
#'   \item{RMSE}{Root mean square error.}
#'   \item{R2}{R-squared \eqn{\left( R^2 \right)}.}
#'   \item{Rbar2}{Adjusted R-squared \eqn{\left( \bar{R}^2 \right)} .}
#' }
#' @export
.model <- function(RSS = NULL,
                   TSS = NULL,
                   n,
                   k,
                   X,
                   y) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (is.null(TSS)) {
    TSS <- TSS(
      y = y
    )
  }
  R2 <- .R2fromRSS(
    RSS = RSS,
    TSS = TSS,
    X = X,
    y = y
  )
  Rbar2 <- .Rbar2(
    R2 = R2,
    n = n,
    k = k,
    X = X,
    y = y
  )
  MSE <- .MSE(
    RSS = RSS,
    n = n,
    X = X,
    y = y
  )
  RMSE <- .RMSE(
    MSE = MSE
  )
  c(
    RSS = RSS,
    MSE = MSE,
    RMSE = RMSE,
    R2 = R2,
    Rbar2 = Rbar2
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model Assessment
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .model
#' @inherit .model return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' model(
#'   X = X,
#'   y = y
#' )
#' @export
model <- function(X,
                  y) {
  .model(
    X = X,
    y = y
  )
}

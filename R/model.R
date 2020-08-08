#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model Assessment
#'
#' @details If `RSS = NULL`, `RSS` is computed using [`RSS()`]
#'   with `X` and `y` as required arguments.
#'   If `RSS` is provided, `X`, and `y` are not needed.
#'   If `TSS = NULL`, `TSS` is computed using [`TSS()`]
#'   with `y` as r equired argument.
#'   If `TSS` is provided, `y` is not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .R2fromRSS
#' @inheritParams .Rbar2
#' @inherit RSS references
#' @inherit MSE references
#' @inherit RMSE references
#' @inherit R2 references
#' @inherit Rbar2 references
#' @return Returns a vector with the following elements
#' \describe{
#'   \item{RSS}{Residual sum of squares.}
#'   \item{MSE}{Mean squared error.}
#'   \item{RMSE}{Root mean squared error.}
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
#' @inherit .model return references
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' model(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' model(X = X, y = y)
#' @export
model <- function(X,
                  y) {
  .model(
    X = X,
    y = y
  )
}

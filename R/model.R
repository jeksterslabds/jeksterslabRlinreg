#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model Assessment
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .R2fromRSS
#' @inheritParams .Rbar2
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
#' @export
model <- function(X,
                  y) {
  .model(
    X = X,
    y = y
  )
}

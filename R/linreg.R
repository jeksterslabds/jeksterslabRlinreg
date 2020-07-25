#' Linear Regression
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @keywords linreg
#' @inheritParams betahat
#' @export
linreg <- function(X,
                   y,
                   qr = TRUE) {
  betahat <- betahat(
    X = X,
    y = y,
    qr = qr
  )
  yhat <- .Xbetahat(
    X = X,
    betahat = betahat,
    y = NULL
  )
  epsilonhat <- .yminusyhat(
    y = y,
    yhat = yhat,
    X = NULL,
    betahat = NULL
  )
  list(
    betahat = betahat,
    yhat = yhat,
    epsilonhat = epsilonhat
  )
}

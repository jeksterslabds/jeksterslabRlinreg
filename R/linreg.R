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
    betahat = betahat
  )
  epsilonhat <- .yminusyhat(
    y = y,
    yhat = yhat
  )
  RSS <- .RSS(
    epsilonhat = epsilonhat
  )
  ESS <- .ESS(
    yhat = yhat,
    ybar = mean(y)
  )
  TSS <- RSS + ESS
  list(
    betahat = betahat,
    yhat = yhat,
    epsilonhat = epsilonhat,
    RSS = RSS,
    ESS = ESS,
    TSS = TSS
  )
}

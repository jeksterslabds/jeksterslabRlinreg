#' Linear Regression
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @keywords linreg
#' @inheritParams betahat
#' @export
linreg <- function(X,
                   y,
                   qr = TRUE) {
  n <- nrow(X)
  k <- ncol(X)
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
  sigma2epsilonhat <- .sigma2epsilonhat(
    RSS = RSS,
    n = n,
    k = k,
    type = "both"
  )
  list(
    betahat = betahat,
    yhat = yhat,
    epsilonhat = epsilonhat,
    RSS = RSS,
    ESS = ESS,
    TSS = TSS,
    sigma2epsilonhat = sigma2epsilonhat
  )
}

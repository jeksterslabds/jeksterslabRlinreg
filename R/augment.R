#' Augment
#'
#' Calculated statistics for each row in the data set.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams betahat
#' @inheritParams RSS
#' @export
augment <- function(X,
                    y,
                    FUN = betahat_inv,
                    betahat = NULL) {
  if (is.null(betahat)) {
    betahat <- betahat(
      X = X,
      y = y,
      FUN = FUN
    )
  }
  yhat <- Xbetahat(
    X = X,
    betahat = betahat,
    y = NULL
  )
  epsilonhat <- y_minus_yhat(
    y = y,
    yhat = yhat,
    X = NULL,
    betahat = betahat
  )
  out <- cbind(
    yhat,
    epsilonhat,
    y,
    X
  )
  Xnames <- paste0(
    "X",
    1:ncol(X)
  )
  colnames(out) <- c(
    "yhat",
    "epsilonhat",
    "y",
    Xnames
  )
  out
}

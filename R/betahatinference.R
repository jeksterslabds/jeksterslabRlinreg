#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients Hypothesis Test and Confidence Intervals
#'
#' @family inference functions
#' @keywords inference
#' @importFrom stats qt pt
#' @inheritParams ci
#' @inheritParams nhst
#' @inheritParams betahat
#' @export
.betahatinference <- function(betahat = NULL,
                              se = NULL,
                              n,
                              X,
                              y) {
  if (is.null(betahat)) {
    betahat <- betahat(
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  if (is.null(se)) {
    se <- sqrt(
      diag(
        vcovbetahat(
          X = X,
          y = y
        )
      )
    )
    n <- nrow(X)
  }
  k <- length(as.vector(betahat))
  nhst <- nhst(
    betahat = betahat,
    se = se,
    n = n,
    k = k
  )
  ci <- ci(
    betahat = betahat,
    se = se,
    n = n,
    k = k
  )
  cbind(
    nhst,
    ci
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Coefficients Hypothesis Test and Confidence Intervals
#'
#' @family inference functions
#' @keywords inference
#' @inheritParams .betahatinference
#' @export
betahatinference <- function(X,
                             y) {
  .betahatinference(
    betahat = NULL,
    se = NULL,
    X = X,
    y = y
  )
}

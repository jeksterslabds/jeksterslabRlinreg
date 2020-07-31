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
#' @return Returns a matrix with the following columns
#' \describe{
#'   \item{coef}{Coefficients.}
#'   \item{se}{Standard error.}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{ci_0.05}{Lower limit 99.99% confidence interval.}
#'   \item{ci_0.5}{Lower limit 99% confidence interval.}
#'   \item{ci_2.5}{Lower limit 95% confidence interval.}
#'   \item{ci_97.5}{Upper limit 95% confidence interval.}
#'   \item{ci_99.5}{Upper limit 99% confidence interval.}
#'   \item{ci_99.95}{Upper limit 99.99% confidence interval.}
#' }
#' @export
.betahatinference <- function(betahat = NULL,
                              sehatbetahat = NULL,
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
  if (is.null(sehatbetahat)) {
    sehatbetahat <- sqrt(
      diag(
        vcovhatbetahat(
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
    sehatbetahat = sehatbetahat,
    n = n,
    k = k
  )
  ci <- ci(
    betahat = betahat,
    sehatbetahat = sehatbetahat,
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
#' @inherit .betahatinference return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' betahatinference(
#'   X = X,
#'   y = y
#' )
#' @export
betahatinference <- function(X,
                             y) {
  .betahatinference(
    betahat = NULL,
    sehatbetahat = NULL,
    X = X,
    y = y
  )
}

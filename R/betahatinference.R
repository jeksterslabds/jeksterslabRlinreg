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
    sehatbetahat <- sehatbetahat(
      X = X,
      y = y
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
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' betahatinference(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' betahatinference(X = X, y = y)
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized Regression Slopes Hypothesis Test and Confidence Intervals
#'
#' @family inference functions
#' @keywords inference
#' @inheritParams .betahatinference
#' @inheritParams .sehatslopesprimetb
#' @param sehatslopesprime Numeric vector of length `p` or `p` by `1` matrix.
#'   Standard errors of estimates of standardized regression slopes.
#' @param sehatslopesprimetype Character string.
#'   Standard errors for standardized regression slopes hypothesis test.
#'   Options are `sehatslopesprimetype = "textbook"` and `sehatslopesprimetype = "delta"`.
#' @param adjust Logical.
#'   If `sehatslopesprimetype = "delta"` and `adjust = TRUE`,
#'   uses `n - 3` to adjust `sehatslopesprime` for bias.
#'   This adjustment is recommended for small sample sizes.
#' @export
.slopesprimeinference <- function(slopesprime = NULL,
                                  sehatslopesprime = NULL,
                                  sehatslopesprimetype = "textbook",
                                  adjust = FALSE,
                                  n,
                                  X,
                                  y) {
  if (is.null(slopesprime)) {
    slopesprime <- slopesprime(
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  if (is.null(sehatslopesprime)) {
    if (sehatslopesprimetype == "textbook") {
      sehatslopesprime <- sehatslopesprimetb(
        X = X,
        y = y
      )
    }
    if (sehatslopesprimetype == "delta") {
      sehatslopesprime <- sehatslopesprimedelta(
        X = X,
        y = y,
        adjust = adjust
      )
    }
    n <- nrow(X)
  }
  out <- .betahatinference(
    betahat = c(0, slopesprime),
    sehatbetahat = c(0, sehatslopesprime),
    n = n
  )
  # for cases when the model is a simple linear regression
  k <- nrow(out)
  p <- k - 1
  varnames <- colnames(out)
  out <- matrix(
    out[-1, ],
    nrow = p
  )
  colnames(out) <- varnames
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized Regression Slopes Hypothesis Test and Confidence Intervals
#'
#' @family inference functions
#' @keywords inference
#' @inheritParams .slopesprimeinference
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopesprimeinference(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' slopesprimeinference(X = X, y = y)
#' @export
slopesprimeinference <- function(X,
                                 y,
                                 sehatslopesprimetype = "textbook") {
  .slopesprimeinference(
    slopesprime = NULL,
    sehatslopesprime = NULL,
    sehatslopesprimetype = sehatslopesprimetype,
    X = X,
    y = y
  )
}

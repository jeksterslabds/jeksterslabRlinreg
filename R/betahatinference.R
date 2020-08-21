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
#' @inheritParams .sehatslopeshatprimetb
#' @param sehatslopeshatprime Numeric vector of length `p` or `p` by `1` matrix.
#'   Standard errors of estimates of standardized regression slopes.
#' @param sehatslopeshatprimetype Character string.
#'   Standard errors for standardized regression slopes hypothesis test.
#'   Options are `sehatslopeshatprimetype = "textbook"` and `sehatslopeshatprimetype = "delta"`.
#' @param adjust Logical.
#'   If `sehatslopeshatprimetype = "delta"` and `adjust = TRUE`,
#'   uses `n - 3` to adjust `sehatslopeshatprime` for bias.
#'   This adjustment is recommended for small sample sizes.
#' @export
.slopeshatprimeinference <- function(slopeshatprime = NULL,
                                     sehatslopeshatprime = NULL,
                                     sehatslopeshatprimetype = "textbook",
                                     adjust = FALSE,
                                     n,
                                     X,
                                     y) {
  if (is.null(slopeshatprime)) {
    slopeshatprime <- slopeshatprime(
      X = X,
      y = y
    )
    n <- nrow(X)
  }
  if (is.null(sehatslopeshatprime)) {
    if (sehatslopeshatprimetype == "textbook") {
      sehatslopeshatprime <- sehatslopeshatprimetb(
        X = X,
        y = y
      )
    }
    if (sehatslopeshatprimetype == "delta") {
      sehatslopeshatprime <- sehatslopeshatprimedelta(
        X = X,
        y = y,
        adjust = adjust
      )
    }
    n <- nrow(X)
  }
  out <- .betahatinference(
    betahat = c(0, slopeshatprime),
    sehatbetahat = c(0, sehatslopeshatprime),
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
#' @inheritParams .slopeshatprimeinference
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopeshatprimeinference(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' slopeshatprimeinference(X = X, y = y)
#' @export
slopeshatprimeinference <- function(X,
                                    y,
                                    sehatslopeshatprimetype = "textbook") {
  .slopeshatprimeinference(
    slopeshatprime = NULL,
    sehatslopeshatprime = NULL,
    sehatslopeshatprimetype = sehatslopeshatprimetype,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Hypothesis Test for Estimates of Regression Coefficients
#'
#' @family hypothesis testing functions
#' @keywords inference
#' @inheritParams ci
#' @return Returns a matrix with the following columns
#' \describe{
#'   \item{coef}{Coefficients.}
#'   \item{se}{Standard error.}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#' }
#' @export
nhst <- function(betahat,
                 sehatbetahat,
                 n,
                 k) {
  df <- n - k
  tstatistic <- betahat / sehatbetahat
  p <- 2 * pt(
    q = -abs(tstatistic),
    df = df,
    lower.tail = TRUE
  )
  cbind(
    coef = betahat,
    se = sehatbetahat,
    t = tstatistic,
    p = p
  )
}

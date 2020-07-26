#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Hypothesis Test for Estimates of Regression Coefficients
#'
#' @family hypothesis testing functions
#' @keywords inference
#' @inheritParams ci
#' @export
nhst <- function(betahat,
                 se,
                 n,
                 k) {
  df <- n - k
  tstatistic <- betahat / se
  p <- 2 * pt(
    q = -abs(tstatistic),
    df = df,
    lower.tail = TRUE
  )
  cbind(
    coef = betahat,
    se = se,
    t = tstatistic,
    p = p
  )
}

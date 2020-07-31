#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Confidence Intervals of Estimates of Regression Coefficients
#'
#' @family hypothesis testing functions
#' @keywords inference
#' @inheritParams .RSS
#' @inheritParams .sigma2hatepsilonhat
#' @param sehatbetahat Numeric vector.
#'   Standard errors of regression coefficients.
#' @return Returns a matrix with the following columns
#' \describe{
#'   \item{ci_0.05}{Lower limit 99.99% confidence interval.}
#'   \item{ci_0.5}{Lower limit 99% confidence interval.}
#'   \item{ci_2.5}{Lower limit 95% confidence interval.}
#'   \item{ci_97.5}{Upper limit 95% confidence interval.}
#'   \item{ci_99.5}{Upper limit 99% confidence interval.}
#'   \item{ci_99.95}{Upper limit 99.99% confidence interval.}
#' }
#' @export
ci <- function(betahat,
               sehatbetahat,
               n,
               k) {
  alpha <- c(0.001, 0.01, 0.05)
  df <- n - k
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  prob <- c(prob_ll, prob_ul)
  tcritical <- qt(
    p = prob,
    df = df
  )
  ci <- matrix(
    data = NA,
    nrow = k,
    ncol = length(tcritical)
  )
  for (i in seq_along(tcritical)) {
    ci[, i] <- betahat + (tcritical[i] * sehatbetahat)
  }
  colnames(ci) <- paste0(
    "ci_",
    prob * 100
  )
  ci
}

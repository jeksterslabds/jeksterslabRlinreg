#' Beta-hat Inference
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param vcov Matrix.
#'   Variance-covariance matrix
#'   of estimates
#'   of regression coefficients.
#' @inheritParams .vcov_betahat
#' @inheritParams betahat
#' @inheritParams .Rbar2
#' @importFrom stats pt
#' @importFrom stats qt
#' @export
.betahat_inf <- function(betahat = NULL,
                         vcov = NULL,
                         n,
                         X,
                         y,
                         FUN = betahat_inv,
                         type = "unbiased") {
  if (is.null(betahat)) {
    betahat <- betahat(
      X = X,
      y = y,
      FUN = FUN
    )
    n <- nrow(X)
  }
  if (is.null(vcov)) {
    vcov <- .vcov_betahat(
      sigma2hat = NULL,
      RSS = NULL,
      betahat = betahat,
      X = X,
      y = y,
      type = type
    )
  }
  k <- length(as.vector(betahat))
  se <- sqrt(diag(vcov))
  t <- betahat / se
  p <- 2 * pt(
    q = t,
    df = n - k,
    lower.tail = FALSE
  )
  alpha <- c(0.001, 0.01, 0.05)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  prob <- c(prob_ll, prob_ul)
  t_critical <- qt(
    p = prob,
    df = n - k
  )
  ci <- matrix(
    data = NA,
    nrow = k,
    ncol = length(t_critical)
  )
  for (i in seq_along(t_critical)) {
    ci[, i] <- betahat + (t_critical[i] * se)
  }
  colnames(ci) <- paste0(
    "ci_",
    prob * 100
  )
  # write tests for betahat_std_mat
  # std <- betahat_std_mat(
  #  X = X,
  #  y = y
  # )
  cbind(
    Coefficients = betahat,
    se = se,
    t = t,
    p = p,
    ci
    # std = std
  )
}


#' Beta-hat Inference
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .betahat_inf
#' @export
betahat_inf <- function(X,
                        y,
                        FUN = betahat_inv,
                        type = "unbiased") {
  .betahat_inf(
    betahat = NULL,
    vcov = NULL,
    X = X,
    y = y,
    FUN = betahat_inv,
    type = "unbiased"
  )
}

#' Linear Regression
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param beta_hat String.
#'   How the regression coefficients are estimated.
#'   `beta_hat = "inv"` for [`beta_hat_inv()`],
#'   `beta_hat = "qr"` for [`beta_hat_qr()`],
#'   `beta_hat = "svd"` for [`beta_hat_svd()`].
#' @inheritParams beta_hat_inv
#' @export
linreg <- function(X,
                   y,
                   beta_hat = "inv") {
  n <- nrow(X)
  k <- ncol(X)
  if (beta_hat == "inv") {
    beta_hat <- beta_hat_inv(
      X = X,
      y = y
    )
  }
  if (beta_hat == "qr") {
    beta_hat <- beta_hat_qr(
      X = X,
      y = y
    )
  }
  if (beta_hat == "svd") {
    beta_hat <- beta_hat_svd(
      X = X,
      y = y
    )
  }
  y_hat <- y_hat_Xbeta_hat(
    X = X,
    beta_hat = beta_hat,
    y = NULL
  )
  e <- e_y_minus_y_hat(
    y = y,
    y_hat = y_hat,
    X = NULL,
    beta_hat = NULL
  )
  rss <- ss_r_e(e = e)
  tss <- ss_t(y = y)
  ess <- tss - rss
  r2 <- r2_rss(
    rss = rss,
    tss = tss
  )
  rbar2 <- rbar2_r2(
    r2 = r2,
    n = n,
    k = k
  )
  list(
    X = X,
    y = y,
    y_hat = y_hat,
    e = e,
    n = n,
    k = k,
    beta_hat = beta_hat,
    rss = rss,
    ess = ess,
    tss = tss,
    r2 = r2,
    rbar2 = rbar2
  )
}

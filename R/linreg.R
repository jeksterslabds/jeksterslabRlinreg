#' Linear Regression
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param output Character vector.
#'   Results to print.
#' @inheritParams betahat
#' @importFrom stats pt
#' @importFrom stats pf
#' @export
linreg <- function(X,
                   y,
                   FUN = betahat_inv,
                   output = c("coef", "model", "anova")) {
  n <- nrow(X)
  k <- ncol(X)
  df1 <- k - 1
  df2 <- n - k
  betahat <- betahat(
    X = X,
    y = y,
    FUN = FUN
  )
  yhat <- Xbetahat(
    X = X,
    betahat = betahat,
    y = NULL
  )
  e <- y_minus_yhat(
    y = y,
    yhat = yhat,
    X = NULL,
    betahat = NULL
  )
  rss <- .rss(e = e)
  tss <- tss(y = y)
  ess <- tss - rss
  ms_model <- ess / df1
  ms_error <- rss / df2
  r2 <- .r2_rss(
    rss = rss,
    tss = tss
  )
  F <- (r2 / (k - 1)) / ((1 - r2) / (n - k))
  Fp <- pf(
    q = F,
    df1 = df1,
    df2 = df2,
    lower.tail = FALSE
  )
  rbar2 <- .rbar2(
    r2 = r2,
    n = n,
    k = k
  )
  mse <- .mse(
    rss = rss,
    n = n
  )
  rmse <- sqrt(mse)
  sigma2hat <- .sigma2hat(
    rss = rss,
    n = n,
    k = k,
    type = "unbiased"
  )
  vcov <- .vcov_betahat(
    sigma2hat = sigma2hat,
    X = X
  )
  se <- sqrt(diag(vcov))
  t <- betahat / se
  p.value <- 2 * pt(
    q = t,
    df = n - k,
    lower.tail = FALSE
  )
  coefficients <- data.frame(
    Coefficients = betahat,
    se = se,
    t = t,
    p = p.value
  )
  model <- matrix(
    data = c(
      r2,
      rbar2,
      mse,
      rmse
    ),
    ncol = 1
  )
  colnames(model) <- c("Value")
  rownames(model) <- c(
    "Coefficient of determination",
    "Adjusted coefficient of determination",
    "Mean Squared Error",
    "Root Mean Squared Error"
  )
  anova <- data.frame(
    Source = c("Model", "Error", "Total"),
    df = c(df1, df2, df1 + df2),
    SS = c(ess, rss, tss),
    MS = c(ms_model, ms_error, NA),
    F = c(F, NA, NA),
    p = c(Fp, NA, NA)
  )
  if ("coef" %in% output) {
    message("Coefficients:")
    print(coefficients)
  }
  if ("model" %in% output) {
    message("\n", "Model Evaluation:")
    print(model)
  }
  if ("anova" %in% output) {
    message("\n", "ANOVA Table:")
    print(anova, row.names = FALSE)
  }
  invisible(
    list(
      X = X,
      y = y,
      yhat = yhat,
      e = e,
      n = n,
      k = k,
      betahat = betahat,
      se = se,
      rss = rss,
      ess = ess,
      tss = tss,
      ms_model = ms_model,
      ms_error = ms_error,
      r2 = r2,
      F = F,
      Fp = Fp,
      rbar2 = rbar2,
      mse = mse,
      rmse = rmse,
      sigma2hat = sigma2hat,
      vcov = vcov,
      coefficients = coefficients,
      model = model,
      anova = anova
    )
  )
}

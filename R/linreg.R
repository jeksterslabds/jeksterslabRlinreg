#' Linear Regression
#'
#' DESCRIPTION
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param output Character vector.
#'   Results to print.
#' @inheritParams betahat
#' @export
linreg <- function(X,
                   y,
                   FUN = betahat_inv,
                   output = c("coef", "ci", "model", "anova")) {
  n <- nrow(X)
  k <- ncol(X)
  df1 <- k - 1
  df2 <- n - k
  descriptives <- descriptives(
    X = X,
    y = y,
    muhat = TRUE
  )
  betahat <- betahat(
    X = X,
    y = y,
    FUN = FUN
  )
  augment <- augment(
    X = X,
    y = y,
    FUN = FUN,
    betahat = betahat
  )
  RSS <- .RSS(
    epsilonhat = augment[, "epsilonhat"]
  )
  ESS <- .ESS(
    yhat = augment[, "yhat"],
    ybar = mean(y)
  )
  TSS <- ESS + RSS
  R2 <- .R2_RSS(
    RSS = RSS,
    TSS = TSS
  )
  Rbar2 <- .Rbar2(
    R2 = R2,
    n = n,
    k = k
  )
  MSE <- .MSE(
    RSS = RSS,
    n = n
  )
  RMSE <- sqrt(MSE)
  model <- c(
    R2,
    Rbar2,
    RSS,
    MSE,
    RMSE
  )
  model <- matrix(
    data = model,
    ncol = 1
  )
  colnames(model) <- "Value"
  rownames(model) <- c(
    "Coefficient of determination",
    "Adjusted Coefficient of determination",
    "Residual Sum of Squares",
    "Mean Square Error",
    "Root Mean Square Error"
  )
  if ("model" %in% output) {
    message(
      paste0(
        "\n",
        "Model Evaluation:"
      )
    )
    print(
      round(
        x = model,
        digits = 4
      )
    )
  }
  # sigma2hat <- .sigma2hat(
  #  RSS = RSS,
  #  n = n,
  #  k = k,
  #  type = "unbiased",
  # )
  # vcov <- .vcov_betahat(
  #  sigma2hat = sigma2hat,
  #  RSS = RSS,
  #  type = "unbiased"
  # )
  # betahat_inf <- .betahat_inf(
  #  betahat = betahat,
  #  vcov = vcov,
  #  n = n
  # )
  # aov <- .aov(
  #  RSS = RSS,
  #  ESS = ESS,
  #  k = k,
  #  n = n
  #  )
  #  list(
  #    descriptives = descriptives,
  #    augment = augment,
  #    coefficients = betahat_inf,
  #    anova = aov
  #  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Linear Regression
#'
#' @keywords linreg
#' @inheritParams betahat
#' @inheritParams descriptives
#' @param unbiased Logical.
#'   Use unbiased standard errors for regression coefficients
#'   hypothesis test.
#' @importFrom graphics par abline
#' @importFrom stats qqnorm qqline cor cov
#' @param plot Logical.
#'   Display plots.
#' @param print Logical.
#'   Display summary output.
#' @export
linreg <- function(X,
                   y,
                   varnamesX = NULL,
                   varnamey = NULL,
                   qr = TRUE,
                   unbiased = TRUE,
                   plot = TRUE,
                   print = TRUE) {
  descriptives <- descriptives(
    X = X,
    y = y,
    varnamesX = varnamesX,
    varnamey = varnamey,
    plot = FALSE,
    msd = FALSE,
    cor = FALSE
  )
  varnamesX <- descriptives[["varnamesX"]]
  varnamesy <- descriptives[["varnamesy"]]
  betahatnames <- c("Intercept", varnamesX[-1])
  # proj--------------------------------------------------------------------------
  P <- P(X)
  M <- .M(
    P = P,
    X = X
  )
  h <- .h(P = P)
  # betahat---------------------------------------------------------------------------
  betahat <- betahat(
    X = X,
    y = y,
    qr = qr
  )
  betahat <- as.vector(betahat)
  names(betahat) <- betahatnames
  # betahat prime-----------------------------------------------------------------------------
  betahatprime <- slopesprime(
    RX = descriptives[["RX"]],
    ryX = descriptives[["ryX"]]
  )
  betahatprime <- as.vector(
    c(
      0,
      betahatprime
    )
  )
  names(betahatprime) <- betahatnames
  # yhat -----------------------------------------------------------------------------
  yhat <- .Xbetahat(
    X = X,
    betahat = betahat
  )
  # yhat <- .Py(y = y, P = P)
  # epsilonhat -----------------------------------------------------------------------------
  epsilonhat <- .yminusyhat(
    y = y,
    yhat = yhat
  )
  # epsilonhat <- .My(y = y, M = M)
  # SS -----------------------------------------------------------------------------
  RSS <- .RSS(
    epsilonhat = epsilonhat
  )
  ESS <- .ESS(
    yhat = yhat,
    ybar = descriptives[["muhaty"]]
  )
  TSS <- RSS + ESS
  # sigma2epsilonhat -----------------------------------------------------------------------------
  sigma2epsilonhat <- .sigma2epsilonhat(
    RSS = RSS,
    n = descriptives[["n"]],
    k = descriptives[["k"]]
  )
  sigma2epsilonhatbiased <- .sigma2epsilonhatbiased(
    RSS = RSS,
    n = descriptives[["n"]]
  )
  # studentized residuals ---------------------------------------------------------------------------------------
  tepsilonhat <- .tepsilonhat(
    epsilonhat = epsilonhat,
    h = h,
    sigma2epsilonhat = sigma2epsilonhat
  )
  # R2 -----------------------------------------------------------------------------
  R2 <- .R2fromRSS(
    RSS = RSS,
    TSS = TSS
  )
  Rbar2 <- .Rbar2(
    R2 = R2,
    n = descriptives[["n"]],
    k = descriptives[["k"]],
  )
  # inference -----------------------------------------------------------------------------
  aov <- .anovatable(
    RSS = RSS,
    ESS = ESS,
    n = descriptives[["n"]],
    k = descriptives[["k"]]
  )
  vcov <- .vcovbetahat(
    sigma2epsilonhat = sigma2epsilonhat,
    X = X
  )
  colnames(vcov) <- betahatnames
  rownames(vcov) <- betahatnames
  vcovbiased <- .vcovbetahatbiased(
    sigma2epsilonhatbiased = sigma2epsilonhatbiased,
    X = X
  )
  colnames(vcovbiased) <- betahatnames
  rownames(vcovbiased) <- betahatnames
  se <- sqrt(diag(vcov))
  names(se) <- betahatnames
  sebiased <- sqrt(diag(vcovbiased))
  names(sebiased) <- betahatnames
  if (unbiased) {
    setouse <- se
  } else {
    setouse <- sebiased
  }
  betahatinference <- .betahatinference(
    betahat = betahat,
    se = setouse,
    n = descriptives[["n"]]
  )
  betahatinference <- cbind(
    betahatinference,
    "std. coef" = betahatprime
  )
  rownames(betahatinference) <- betahatnames
  ci <- betahatinference
  ci <- ci[, -c(1, 2, 3, 4, ncol(betahatinference))]
  coefficients <- betahatinference[, c(1, 2, 3, 4, ncol(betahatinference))]
  if (print) {
    # display -------------------------------------------------------------------------------------------
    ## anova table--------------------------------------------------------------------------------------
    cat("\nANOVA Table:\n")
    print(
      aov
    )
    ## coefficients--------------------------------------------------------------------------------------
    cat("\nCoefficients:\n")
    if (!unbiased) {
      cat("Biased standard errors are used.\n")
    }
    print(
      coefficients
    )
    ## confidence intervals --------------------------------------------------------------------------------------
    cat("\nConfidence Intervals:\n")
    print(
      ci
    )
    ## descriptive statistics--------------------------------------------------------------------------------------
    cat("\nMeans and Standard Deviations:\n")
    print(
      descriptives[["meanandsd"]]
    )
  }
  ## plots--------------------------------------------------------------------------------------
  if (plot) {
    scatter.plot(
      data = descriptives[["data"]]
    )
    residual.plot(
      yhat = yhat,
      tepsilonhat = tepsilonhat,
      epsilonhat = epsilonhat
    )
  }
  out <- list(
    P = P,
    M = M,
    h = h,
    betahat = betahat,
    betahatprime = betahatprime,
    yhat = yhat,
    epsilonhat = epsilonhat,
    tepsilonhat = tepsilonhat,
    RSS = RSS,
    ESS = ESS,
    TSS = TSS,
    sigma2epsilonhat = sigma2epsilonhat,
    sigma2epsilonhatbiased = sigma2epsilonhatbiased,
    R2 = R2,
    Rbar2 = Rbar2,
    MSmodel = aov["Model", "MS"],
    MSerror = aov["Error", "MS"],
    F = aov["Model", "F"],
    pf = aov["Model", "p"],
    vcov = vcov,
    vcovbiased = vcovbiased,
    se = se,
    sebiased = sebiased,
    t = betahatinference[, "t"],
    pt = betahatinference[, "p"],
    coefficients = coefficients,
    ci = ci,
    betahatinference = betahatinference
  )
  invisible(
    c(
      descriptives,
      out
    )
  )
}

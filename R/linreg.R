#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Linear Regression
#'
#' @keywords linreg
#' @inheritParams betahat
#' @inheritParams descriptives
#' @inheritParams .slopeshatprimeinference
#' @param sehatbetahattype Character string.
#'   Standard errors for regression coefficients hypothesis test.
#'   Options are `sehatbetahattype = "unbiased"` and `sehatbetahattype = "biased"`.
#' @importFrom graphics par abline
#' @importFrom stats qqnorm qqline cor cov
#' @param plot Logical.
#'   Display plots.
#' @param print Logical.
#'   Display summary output.
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' linreg(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' linreg(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' # delta standard errors for standardized coefficients
#' linreg(X = X, y = y, sehatslopeshatprimetype = "delta")
#' @export
linreg <- function(X,
                   y,
                   varnamesX = NULL,
                   varnamey = NULL,
                   qr = TRUE,
                   sehatbetahattype = "unbiased",
                   sehatslopeshatprimetype = "delta",
                   adjust = FALSE,
                   plot = TRUE,
                   print = TRUE) {
  # descriptives--------------------------------------------------------------------------
  descriptives <- descriptives(
    X = X,
    y = y,
    varnamesX = varnamesX,
    varnamey = varnamey,
    plot = FALSE,
    moments = FALSE,
    cor = FALSE,
    mardia = TRUE
  )
  X <- descriptives[["X"]]
  y <- descriptives[["y"]]
  varnamesX <- colnames(X)
  varnamey <- colnames(y)
  betahatnames <- c("Intercept", varnamesX[-1])
  RXhat <- descriptives[["RXhat"]]
  ryXhat <- descriptives[["ryXhat"]]
  SigmaXhat <- descriptives[["SigmaXhat"]]
  sigmayXhat <- descriptives[["sigmayXhat"]]
  sigma2yhat <- descriptives[["sigma2yhat"]]
  ybar <- descriptives[["muhaty"]]
  n <- descriptives[["n"]]
  k <- descriptives[["k"]]
  p <- descriptives[["p"]]
  muhat <- descriptives[["muhat"]]
  sigmahat <- descriptives[["sigmahat"]]
  data <- descriptives[["data"]]
  # proj--------------------------------------------------------------------------
  P <- P(X)
  M <- .M(
    P = P,
    X = X
  )
  h <- .h(P = P)
  # betahat---------------------------------------------------------------------------
  betahat <- as.vector(
    betahat(
      X = X,
      y = y,
      qr = qr
    )
  )
  names(betahat) <- betahatnames
  slopeshat <- betahat[-1]
  # betahat prime-----------------------------------------------------------------------------
  slopeshatprime <- .slopeshatprime(
    RXhat = RXhat,
    ryXhat = ryXhat
  )
  slopeshatprime <- as.vector(slopeshatprime)
  betahatprime <- as.vector(
    c(
      0,
      slopeshatprime
    )
  )
  names(betahatprime) <- betahatnames
  names(slopeshatprime) <- varnamesX[-1]
  # yhat-----------------------------------------------------------------------------
  yhat <- .Xbetahat(
    X = X,
    betahat = betahat
  )
  # yhat <- .Py(y = y, P = P)
  # epsilonhat-----------------------------------------------------------------------------
  epsilonhat <- .yminusyhat(
    y = y,
    yhat = yhat
  )
  # epsilonhat <- .My(y = y, M = M)
  # SS-----------------------------------------------------------------------------
  RSS <- .RSS(
    epsilonhat = epsilonhat
  )
  ESS <- .ESS(
    yhat = yhat,
    ybar = ybar
  )
  TSS <- RSS + ESS
  # sigma2hatepsilonhat-----------------------------------------------------------------------------
  sigma2hatepsilonhat <- .sigma2hatepsilonhat(
    RSS = RSS,
    n = n,
    k = k
  )
  sigma2hatepsilonhatbiased <- .sigma2hatepsilonhatbiased(
    RSS = RSS,
    n = n
  )
  # studentized residuals---------------------------------------------------------------------------------------
  tepsilonhat <- .tepsilonhat(
    epsilonhat = epsilonhat,
    h = h,
    sigma2hatepsilonhat = sigma2hatepsilonhat
  )
  # R2-----------------------------------------------------------------------------
  R2 <- .R2fromRSS(
    RSS = RSS,
    TSS = TSS
  )
  Rbar2 <- .Rbar2(
    R2 = R2,
    n = n,
    k = k
  )
  # mse---------------------------------------------------------------------------------------
  MSE <- .MSE(
    RSS = RSS,
    n = n
  )
  # rmse----------------------------------------------------------------------------------
  RMSE <- .RMSE(
    MSE = MSE
  )
  # anovatable-----------------------------------------------------------------------------
  anovatable <- .anovatable(
    RSS = RSS,
    ESS = ESS,
    n = n,
    k = k
  )
  # vcovhat-----------------------------------------------------------------------------
  vcovhatbetahat <- .vcovhatbetahat(
    sigma2hatepsilonhat = sigma2hatepsilonhat,
    X = X
  )
  colnames(vcovhatbetahat) <- betahatnames
  rownames(vcovhatbetahat) <- betahatnames
  vcovhatbetahatbiased <- .vcovhatbetahatbiased(
    sigma2hatepsilonhatbiased = sigma2hatepsilonhatbiased,
    X = X
  )
  colnames(vcovhatbetahatbiased) <- betahatnames
  rownames(vcovhatbetahatbiased) <- betahatnames
  # sehat-----------------------------------------------------------------------------
  sehatbetahat <- as.vector(
    .sehatbetahat(
      vcovhatbetahat = vcovhatbetahat
    )
  )
  names(sehatbetahat) <- betahatnames
  sehatbetahatbiased <- as.vector(
    .sehatbetahatbiased(
      vcovhatbetahatbiased = vcovhatbetahatbiased
    )
  )
  names(sehatbetahatbiased) <- betahatnames
  sehatslopeshatprimetb <- as.vector(
    .sehatslopeshatprimetb(
      slopeshat = slopeshat,
      sehatslopeshat = sehatbetahat[-1],
      slopeshatprime = slopeshatprime
    )
  )
  names(sehatslopeshatprimetb) <- betahatnames[-1]
  sehatslopeshatprimedelta <- as.vector(
    .sehatslopeshatprimedelta(
      slopeshat = slopeshat,
      sigma2hatepsilonhat = sigma2hatepsilonhat,
      SigmaXhat = SigmaXhat,
      sigmayXhat = sigmayXhat,
      sigma2yhat = sigma2yhat,
      adjust = adjust,
      n = n
    )
  )
  names(sehatslopeshatprimedelta) <- betahatnames[-1]
  # betahatinference----------------------------------------------------------------------------------
  if (sehatbetahattype == "unbiased") {
    sehatbetahattouse <- sehatbetahat
  }
  if (sehatbetahattype == "biased") {
    sehatbetahattouse <- sehatbetahatbiased
  }
  betahatinference <- .betahatinference(
    betahat = betahat,
    sehatbetahat = sehatbetahattouse,
    n = n
  )
  rownames(betahatinference) <- betahatnames
  ci <- betahatinference
  ci <- ci[, -c(1, 2, 3, 4)]
  coefficients <- betahatinference[, c(1, 2, 3, 4)]
  # betahatprimeinference----------------------------------------------------------------------------------
  varnamesbetahatprimeinference <- colnames(betahatinference)
  varnamesbetahatprimeinferenceci <- varnamesbetahatprimeinference[-c(1, 2, 3, 4)]
  varnamesbetahatprimeinferencecoefficients <- varnamesbetahatprimeinference[c(1, 2, 3, 4)]
  if (sehatslopeshatprimetype == "textbook") {
    sehatslopeshatprimetouse <- sehatslopeshatprimetb
  }
  if (sehatslopeshatprimetype == "delta") {
    sehatslopeshatprimetouse <- sehatslopeshatprimedelta
  }
  slopeshatprimeinference <- .slopeshatprimeinference(
    slopeshatprime = slopeshatprime,
    sehatslopeshatprime = sehatslopeshatprimetouse,
    sehatslopeshatprimetype = sehatslopeshatprimetype,
    n = n
  )
  rownames(slopeshatprimeinference) <- betahatnames[-1]
  stdci <- slopeshatprimeinference
  stdci <- stdci[, -c(1, 2, 3, 4)]
  stdci <- matrix(
    data = stdci,
    nrow = p
  )
  rownames(stdci) <- betahatnames[-1]
  colnames(stdci) <- varnamesbetahatprimeinferenceci
  stdcoefficients <- slopeshatprimeinference[, c(1, 2, 3, 4)]
  stdcoefficients <- matrix(
    data = stdcoefficients,
    nrow = p
  )
  rownames(stdcoefficients) <- betahatnames[-1]
  colnames(stdcoefficients) <- varnamesbetahatprimeinferencecoefficients
  if (print) {
    # display-------------------------------------------------------------------------------------------
    ## model assessment--------------------------------------------------------------------------------
    model <- matrix(
      data = round(
        c(
          RSS,
          MSE,
          RMSE,
          R2,
          Rbar2
        ),
        digits = 2
      ),
      ncol = 1
    )
    rownames(model) <- c(
      "RSS",
      "MSE",
      "RMSE",
      "R-squared",
      "Adj. R-squared"
    )
    colnames(model) <- "Value"
    cat("\nModel Assessment:\n")
    print(
      model
    )
    ## anova table--------------------------------------------------------------------------------------
    cat("\nANOVA Table:\n")
    print(
      anovatable
    )
    ## coefficients--------------------------------------------------------------------------------------
    cat("\nCoefficients:\n")
    if (sehatbetahattype == "biased") {
      cat("Biased standard errors are used.\n")
    }
    print(
      coefficients
    )
    cat("\nStandardized Coefficients:\n")
    if (sehatslopeshatprimetype == "textbook") {
      cat("Textbook standard errors are used.\n")
    }
    if (sehatslopeshatprimetype == "delta") {
      cat("Yuan and Chan 2011 standard errors are used.\n")
    }
    print(
      stdcoefficients
    )
    ## confidence intervals--------------------------------------------------------------------------------------
    cat("\nConfidence Intervals - Regression Coefficients:\n")
    print(
      ci
    )
    cat("\nConfidence Intervals - Standardized Slopes:\n")
    print(
      stdci
    )
    ## descriptive statistics--------------------------------------------------------------------------------------
    meanandsd <- cbind(
      muhat,
      sigmahat
    )
    colnames(meanandsd) <- c(
      "Mean",
      "SD"
    )
    cat("\nMeans and Standard Deviations:\n")
    print(
      meanandsd
    )
  }
  ## plots--------------------------------------------------------------------------------------
  if (plot) {
    scatter.plot(
      X = X,
      y = y
    )
    .residual.plot(
      yhat = yhat,
      tepsilonhat = tepsilonhat,
      epsilonhat = epsilonhat,
      h = h
    )
  }
  out <- list(
    P = P,
    M = M,
    h = h,
    betahat = betahat,
    slopeshat = slopeshat,
    betahatprime = betahatprime,
    slopeshatprime = slopeshatprime,
    yhat = yhat,
    epsilonhat = epsilonhat,
    tepsilonhat = tepsilonhat,
    RSS = RSS,
    ESS = ESS,
    TSS = TSS,
    sigma2hatepsilonhat = sigma2hatepsilonhat,
    sigma2hatepsilonhatbiased = sigma2hatepsilonhatbiased,
    R2 = R2,
    Rbar2 = Rbar2,
    MSmodel = anovatable["Model", "MS"],
    MSerror = anovatable["Error", "MS"],
    F = anovatable["Model", "F"],
    F.p = anovatable["Model", "p"],
    MSE = MSE,
    RMSE = RMSE,
    vcovhatbetahat = vcovhatbetahat,
    vcovhatbetahatbiased = vcovhatbetahatbiased,
    sehatbetahat = sehatbetahat,
    sehatbetahatbiased = sehatbetahatbiased,
    sehatslopeshatprimetb = sehatslopeshatprimetb,
    sehatslopeshatprimedelta = sehatslopeshatprimedelta,
    t = betahatinference[, "t"],
    t.p = betahatinference[, "p"],
    coefficients = coefficients,
    stdcoefficients = stdcoefficients,
    ci = ci,
    stdci = stdci
  )
  invisible(
    c(
      descriptives,
      out
    )
  )
}

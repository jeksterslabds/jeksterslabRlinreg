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
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' linreg(
#'   X = X,
#'   y = y
#' )
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
  X <- descriptives[["X"]]
  y <- descriptives[["y"]]
  varnamesX <- colnames(X)
  varnamey <- colnames(y)
  betahatnames <- c("Intercept", varnamesX[-1])
  RX <- descriptives[["RX"]]
  ryX <- descriptives[["ryX"]]
  ybar <- descriptives[["muhaty"]]
  n <- descriptives[["n"]]
  k <- descriptives[["k"]]
  mu <- descriptives[["mu"]]
  sigma <- descriptives[["sigma"]]
  data <- descriptives[["data"]]
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
  betahatprime <- .slopesprime(
    RX = RX,
    ryX = ryX
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
    ybar = ybar
  )
  TSS <- RSS + ESS
  # sigma2hatepsilonhat -----------------------------------------------------------------------------
  sigma2hatepsilonhat <- .sigma2hatepsilonhat(
    RSS = RSS,
    n = n,
    k = k
  )
  sigma2hatepsilonhatbiased <- .sigma2hatepsilonhatbiased(
    RSS = RSS,
    n = n
  )
  # studentized residuals ---------------------------------------------------------------------------------------
  tepsilonhat <- .tepsilonhat(
    epsilonhat = epsilonhat,
    h = h,
    sigma2hatepsilonhat = sigma2hatepsilonhat
  )
  # R2 -----------------------------------------------------------------------------
  R2 <- .R2fromRSS(
    RSS = RSS,
    TSS = TSS
  )
  Rbar2 <- .Rbar2(
    R2 = R2,
    n = n,
    k = k
  )
  # mse ----------------------------------------------------------------------------------------
  MSE <- .MSE(
    RSS = RSS,
    n = n
  )
  # rmse ----------------------------------------------------------------------------------
  RMSE <- .RMSE(
    MSE = MSE
  )
  # inference -----------------------------------------------------------------------------
  anovatable <- .anovatable(
    RSS = RSS,
    ESS = ESS,
    n = n,
    k = k
  )
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
  sehatbetahat <- .sehatbetahat(vcovhatbetahat = vcovhatbetahat)
  names(sehatbetahat) <- betahatnames
  sehatbetahatbiased <- .sehatbetahatbiased(vcovhatbetahatbiased = vcovhatbetahatbiased)
  names(sehatbetahatbiased) <- betahatnames
  sehatbetahatprimebiased <- .sehatbetahatprimebiased(
    betahat = betahat,
    sehatbetahat = sehatbetahat,
    betahatprime = betahatprime
  )
  names(sehatbetahatprimebiased) <- betahatnames
  if (unbiased) {
    sehatbetahattouse <- sehatbetahat
  } else {
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
  betahatprimeinference <- .betahatinference(
    betahat = betahatprime,
    sehatbetahat = sehatbetahatprimebiased,
    n = n
  )
  rownames(betahatprimeinference) <- betahatnames
  stdci <- betahatprimeinference
  stdci <- stdci[, -c(1, 2, 3, 4)]
  stdci <- stdci[2:length(betahatnames), ]
  stdcoefficients <- betahatprimeinference[, c(1, 2, 3, 4)]
  stdcoefficients <- stdcoefficients[2:length(betahatnames), ]
  if (print) {
    # display -------------------------------------------------------------------------------------------
    ## model assessment --------------------------------------------------------------------------------
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
    if (!unbiased) {
      cat("Biased standard errors are used.\n")
    }
    print(
      coefficients
    )
    cat("\nStandardized Coefficients:\n")
    print(
      stdcoefficients
    )
    ## confidence intervals --------------------------------------------------------------------------------------
    cat("\nConfidence Intervals:\n")
    print(
      ci
    )
    cat("\nConfidence Intervals - Standardized Coefficients:\n")
    print(
      stdci
    )
    ## descriptive statistics--------------------------------------------------------------------------------------
    meanandsd <- cbind(
      mu,
      sigma
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
      data = data
    )
    residual.plot(
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
    betahatprime = betahatprime,
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
    pf = anovatable["Model", "p"],
    MSE = MSE,
    RMSE = RMSE,
    vcovhatbetahat = vcovhatbetahat,
    vcovhatbetahatbiased = vcovhatbetahatbiased,
    sehatbetahat = sehatbetahat,
    sehatbetahatbiased = sehatbetahatbiased,
    sehatbetahatprimebiased = sehatbetahatprimebiased,
    t = betahatinference[, "t"],
    pt = betahatinference[, "p"],
    coefficients = coefficients,
    stdcoefficients = stdcoefficients,
    ci = ci
  )
  invisible(
    c(
      descriptives,
      out
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Descriptive Statistics
#'
#' @keywords descriptives
#' @importFrom stats cov cor cor.test
#' @importFrom jeksterslabRdist skew kurt mardia
#' @inheritParams betahat
#' @param plot Logical.
#'   Display scatter plot matrix.
#' @param varnamesX Optional. Character vector of length `k`.
#'   Variable names for matrix `X`.
#' @param varnamey Optional. Character string.
#'   Variable name for vector `y`.
#' @param moments Logical.
#'   Print central moments (means, standard deviations, skewness, and kurtosis).
#' @param cor Logical.
#'   Print correlations.
#' @param mardia Logical.
#'   Estimate Mardia's multivariate skewness and kurtosis.
#' @return Returns descriptive statistics useful in k-variable linear regression model.
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' out <- descriptives(X = X, y = y)
#' str(out)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' out <- descriptives(X = X, y = y)
#' str(out)
#' @export
descriptives <- function(X,
                         y,
                         varnamesX = NULL,
                         varnamey = NULL,
                         plot = TRUE,
                         moments = TRUE,
                         cor = TRUE,
                         mardia = TRUE) {
  n <- nrow(X)
  k <- ncol(X)
  p <- k - 1
  df1 <- k - 1
  df2 <- n - k
  if (is.null(varnamesX)) {
    if (is.null(colnames(X))) {
      varnamesX <- paste0("X", 1:k)
    } else {
      varnamesX <- colnames(X)
    }
  }
  colnames(X) <- varnamesX
  if (is.null(varnamey)) {
    if (is.null(colnames(y))) {
      varnamey <- "y"
    } else {
      varnamey <- colnames(y)
    }
  }
  X <- as.matrix(X)
  y <- matrix(
    data = y,
    ncol = 1
  )
  colnames(y) <- varnamey
  muhatX <- as.vector(colMeans(X))
  muhatX <- muhatX[-1]
  names(muhatX) <- varnamesX[-1]
  muhaty <- mean(y)
  names(muhaty) <- varnamey
  data <- X[, -1]
  data <- cbind(
    y,
    data
  )
  colnames(data) <- c(varnamey, varnamesX[-1])
  Rhat <- cor(data)
  Rhat.p <- matrix(
    data = NA,
    ncol = ncol(data),
    nrow = ncol(data)
  )
  for (j in 1:ncol(data)) {
    for (i in 1:ncol(data)) {
      out <- cor.test(
        data[, i],
        data[, j]
      )
      Rhat.p[i, j] <- out$p.value
    }
  }
  colnames(Rhat.p) <- c(varnamey, varnamesX[-1])
  rownames(Rhat.p) <- c(varnamey, varnamesX[-1])
  diag(Rhat.p) <- rep(x = NA, length = nrow(Rhat.p))
  if (nrow(Rhat) > 2) {
    RXhat <- Rhat[2:k, 2:k]
  } else {
    RXhat <- Rhat[2, 2]
  }
  ryXhat <- as.vector(Rhat[, 1])
  ryXhat <- ryXhat[-1]
  names(ryXhat) <- varnamesX[-1]
  Sigmahat <- cov(data)
  if (nrow(Sigmahat) > 2) {
    SigmaXhat <- Sigmahat[2:k, 2:k]
    sigma2Xhat <- diag(SigmaXhat)
  } else {
    SigmaXhat <- Sigmahat[2, 2]
    sigma2Xhat <- SigmaXhat
  }
  sigmayXhat <- as.vector(Sigmahat[, 1])
  sigma2yhat <- sigmayXhat[1]
  names(sigma2yhat) <- varnamey
  sigmayXhat <- sigmayXhat[-1]
  names(sigmayXhat) <- varnamesX[-1]
  muhat <- c(
    muhaty,
    muhatX
  )
  names(muhat) <- c(varnamey, varnamesX[-1])
  sigma2hat <- c(
    sigma2yhat,
    sigma2Xhat
  )
  names(sigma2hat) <- c(varnamey, varnamesX[-1])
  sigmahat <- sqrt(sigma2hat)
  names(sigmahat) <- c(varnamey, varnamesX[-1])
  sigmayhat <- sigmahat[1]
  sigmaXhat <- sigmahat[-1]
  skewhat <- as.vector(
    apply(
      X = data,
      MARGIN = 2,
      FUN = skew
    )
  )
  names(skewhat) <- c(varnamey, varnamesX[-1])
  kurthat <- as.vector(
    apply(
      X = data,
      MARGIN = 2,
      FUN = kurt
    )
  )
  names(kurthat) <- c(varnamey, varnamesX[-1])
  if (mardia) {
    mardiahat <- mardia(data)
  } else {
    mardiahat <- NA
  }
  if (moments) {
    meanandsd <- cbind(
      muhat,
      sigmahat,
      skewhat,
      kurthat
    )
    colnames(meanandsd) <- c(
      "Mean",
      "SD",
      "Skewness",
      "Kurtosis"
    )
    cat("\nCentral Moments:\n")
    print(
      meanandsd
    )
    if (mardia) {
      cat("\nMardia's Estimate of Multivariate Skewness and Kurtosis:\n")
      print(
        mardiahat
      )
    }
  }
  if (cor) {
    cat("\nCorrelations:\n")
    print(Rhat)
  }
  if (plot) {
    scatter.plot(
      X = X,
      y = y
    )
  }
  invisible(
    list(
      X = X,
      y = y,
      data = data,
      n = n,
      k = k,
      p = p,
      df1 = df1,
      df2 = df2,
      muhatX = muhatX,
      muhaty = muhaty,
      muhat = muhat,
      Rhat = Rhat,
      Rhat.p = Rhat.p,
      RXhat = RXhat,
      ryXhat = ryXhat,
      Sigmahat = Sigmahat,
      SigmaXhat = SigmaXhat,
      sigmayXhat = sigmayXhat,
      sigma2Xhat = sigma2Xhat,
      sigma2yhat = sigma2yhat,
      sigmaXhat = sigmaXhat,
      sigmayhat = sigmayhat,
      sigma2hat = sigma2hat,
      sigmahat = sigmahat,
      skewhat = skewhat,
      kurthat = kurthat,
      mardiahat = mardiahat
    )
  )
}

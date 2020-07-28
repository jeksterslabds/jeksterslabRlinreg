#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Descriptive Statistics
#'
#' @keywords descriptives
#' @inheritParams betahat
#' @param plot Logical.
#'   Display scatter plot matrix.
#' @param varnamesX Character vector of length `k`.
#'   Variable names for matrix `X`.
#' @param varnamey Character string.
#'   Variable name for vector `y`.
#' @param msd Logical.
#'   Print means and standard deviations.
#' @param cor Logical.
#'   Print correlations.
#' @export
descriptives <- function(X,
                         y,
                         varnamesX = NULL,
                         varnamey = NULL,
                         plot = TRUE,
                         msd = TRUE,
                         cor = TRUE) {
  n <- nrow(X)
  k <- ncol(X)
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
  R <- cor(data)
  if (nrow(R) > 2) {
    RX <- R[2:k, 2:k]
  } else {
    RX <- R[2, 2]
  }
  ryX <- as.vector(R[, 1])
  ryX <- ryX[-1]
  Sigma <- cov(data)
  if (nrow(Sigma) > 2) {
    SigmaX <- Sigma[2:k, 2:k]
    sigma2X <- diag(SigmaX)
  } else {
    SigmaX <- Sigma[2, 2]
    sigma2X <- SigmaX
  }
  sigmayX <- as.vector(Sigma[, 1])
  sigma2y <- sigmayX[1]
  sigmayX <- sigmayX[-1]
  mu <- c(
    muhaty,
    muhatX
  )
  names(mu) <- c(varnamey, varnamesX[-1])
  sigma2 <- c(
    sigma2y,
    sigma2X
  )
  names(sigma2) <- c(varnamey, varnamesX[-1])
  sigma <- sqrt(sigma2)
  names(sigma) <- c(varnamey, varnamesX[-1])
  if (msd) {
    meanandsd <- cbind(
      mu,
      sigma
    )
    colnames(meanandsd) <- c(
      "Mean",
      "SD"
    )
    print(
      meanandsd
    )
  }
  if (cor) {
    print(R)
  }
  if (plot) {
    scatter.plot(data)
  }
  invisible(
    list(
      X = X,
      y = y,
      data = data,
      n = n,
      k = k,
      df1 = df1,
      df2 = df2,
      muhatX = muhatX,
      muhaty = muhaty,
      mu = mu,
      R = R,
      RX = RX,
      ryX = ryX,
      Sigma = Sigma,
      SigmaX = SigmaX,
      sigmayX = sigmayX,
      sigma2X = sigma2X,
      sigma2y = sigma2y,
      sigma2 = sigma2,
      sigma = sigma
    )
  )
}

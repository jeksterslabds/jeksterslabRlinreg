#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Error Variance \eqn{\sigma^2}
#'
#' @description Error variance \eqn{\sigma^2} from the slopes
#'   \eqn{\boldsymbol{\beta}_{2 \cdots k}}
#'   and variance-covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams slopes
#' @inheritParams Sigmatheta
#' @param sigma2y Numeric.
#'   Variance of the regressand variable \eqn{\left( \sigma_{y}^{2} \right)}.
#' @export
sigma2epsilon <- function(slopes,
                          sigma2y,
                          sigmayX,
                          SigmaX) {
  foo <- function(A,
                  S) {
    invIminusA <- solve(diag(nrow(A)) - A)
    invIminusA %*% S %*% t(invIminusA)
  }
  slopes <- as.vector(slopes)
  p <- length(slopes)
  k <- p + 1
  A <- matrix(
    data = 0,
    nrow = k,
    ncol = k
  )
  A[1, ] <- c(
    0,
    slopes
  )
  filter <- diag(nrow(A))
  Sigmatheta <- cbind(0, SigmaX)
  Sigmatheta <- rbind(0, Sigmatheta)
  Sigmatheta[1:k, 1] <- c(sigma2y, sigmayX)
  Sigmatheta[1, 1:k] <- c(sigma2y, sigmayX)
  # get variances
  sigma2 <- diag(Sigmatheta)
  # construct S matrix
  ## from Sigmatheta
  S <- Sigmatheta
  ## zeroes on y row and column
  S[1, ] <- 0
  S[, 1] <- 0
  ## zeroes on diagonal
  diag(S) <- 0
  ## initial Sigmatheta
  Sigmatheta_temp <- foo(
    A = A,
    S = S
  )
  for (i in k:1) {
    S[i, i] <- sigma2[i] - Sigmatheta_temp[i, i]
    Sigmatheta_temp <- foo(
      A = A,
      S = S
    )
  }
  S[1, 1]
}

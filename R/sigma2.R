#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Error Variance \eqn{\sigma^2}
#'
#' @description Error variance \eqn{\sigma^2} from the slopes
#'   \eqn{\boldsymbol{\beta}_{2 \cdots k}}
#'   and variance-covariance matrix \eqn{\boldsymbol{\Sigma}}.
#'
#' @family model-implied functions
#' @keywords model-implied, parameters
#' @inheritParams slopes
#' @inheritParams Sigmatheta
sigma2 <- function(slopes,
                   sigmayX,
                   SigmaX) {
  foo <- function(Sigmatheta,
                  A) {
    invIminusA <- solve(I - A)
    invIminusA %*% Sigmatheta %*% t(invIminusA)
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
  Sigmatheta <- cbind(0, SigmaX)
  Sigmatheta <- rbind(0, Sigmatheta)
  Sigmatheta[1, ] <- sigmayX
  Sigmatheta[, 1] <- sigmayX
  S <- foo(
    Sigmatheta = Sigmatheta,
    A = A
  )
  S[1, 1]
}

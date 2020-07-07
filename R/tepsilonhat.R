.tepsilonhat <- function(epsilonhat = NULL,
                         h = NULL,
                         sigma2hat = NULL,
                         X = NULL,
                         y = NULL,
                         betahat = NULL) {
  if (is.null(epsilonhat)) {
    epsilonhat <- epsilonhat(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  if (is.null(h)) {
    h <- h(X)
  }
  if (is.null(sigma2hat)) {
    sigma2hat <- sigma2hat(
      X = X,
      y = y,
      type = "unbiased"
    )
  }
  epsilonhat / sqrt(sigma2hat * (1 - h))
}


tepsilonhat <- function(X,
                        y,
                        betahat = NULL) {
  .tepsilonhat(
    epsilonhat = NULL,
    h = NULL,
    sigma2hat = NULL,
    X = X,
    y = y,
    betahat = betahat
  )
}

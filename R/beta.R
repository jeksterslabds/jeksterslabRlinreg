#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'
#' @description Derives the slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'   of a linear regression model (\eqn{\boldsymbol{\beta}} minus the intercept)
#'   as a function of covariances.
#'
#' @details The linear regression slopes is given by
#'   \deqn{
#'     \boldsymbol{\beta}_{2, \cdots, k} =
#'     \mathrm{V}_{\mathbf{X}}^{T} \mathrm{v}_{\mathbf{yX}}
#'   }
#'
#'   where
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}}
#'     is the \eqn{p \times p} covariance matrix of the regressor variables and
#'   - \eqn{\boldsymbol{\sigma}_{\mathbf{yX}}}
#'     is the \eqn{p \times 1} column vector
#'     of the covariances between the regressand and the regressors.
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams Sigmatheta
#' @inheritParams betahat
#' @param sigmayX Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} covariances between the regressand and the regressors
#'   \eqn{\left( \boldsymbol{\sigma}_{\mathbf{yX}} \right)}.
#' @return Returns the slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'   of a linear regression model derived from the variance-covariance matrix.
#' @export
.slopes <- function(SigmaX = NULL,
                    sigmayX = NULL,
                    X,
                    y) {
  if (is.null(SigmaX) | is.null(sigmayX)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      plot = FALSE,
      moments = FALSE,
      cor = FALSE
    )
    SigmaX <- descriptives[["SigmaX"]]
    sigmayX <- descriptives[["sigmayX"]]
  }
  out <- solve(SigmaX) %*% sigmayX
  colnames(out) <- "slopes"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams .slopes
#' @inherit .slopes description details return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopes(X = X, y = y)
#' @export
slopes <- function(X,
                   y) {
  .slopes(
    SigmaX = NULL,
    sigmayX = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Standardized Slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}^{\prime}}
#'
#' @description Derives the standardized slopes
#'   \eqn{\boldsymbol{\beta}_{2, \cdots, k}^{\prime}} of a linear regression model
#'   as a function of correlations.
#'
#' @details The linear regression standardized slopes is given by
#'   \deqn{
#'     \boldsymbol{\beta}_{2, \cdots, k}^{\prime} =
#'     \mathrm{R}_{\mathbf{X}}^{T} \mathrm{r}_{\mathbf{yX}}
#'   }
#'
#'   where
#'   - \eqn{\mathbf{R}_{\mathbf{X}}}
#'     is the \eqn{p \times p} correlation matrix of the regressor variables and
#'   - \eqn{\mathbf{r}_{\mathbf{yX}}}
#'     is the \eqn{p \times 1} column vector
#'     of the correlations between the regressand and the regressors.
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams betahat
#' @param RX `p` by `p` numeric matrix.
#'   \eqn{p \times p} correlations between the regressors.
#' @param ryX Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} correlations between the regressand and the regressors.
#' @return Returns the standardized slopes
#'   \eqn{\boldsymbol{\beta}_{2, \cdots, k}^{\prime}}
#'   of a linear regression model derived from the correlation matrix.
#' @export
.slopesprime <- function(RX = NULL,
                         ryX = NULL,
                         X,
                         y) {
  if (is.null(RX) | is.null(ryX)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      plot = FALSE,
      moments = FALSE,
      cor = FALSE
    )
    RX <- descriptives[["RX"]]
    ryX <- descriptives[["ryX"]]
  }
  out <- solve(RX) %*% ryX
  colnames(out) <- "std. slopes"
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Standardized Slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}^{\prime}}
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams .slopesprime
#' @inherit .slopesprime description details return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopesprime(X = X, y = y)
#' @export
slopesprime <- function(X,
                        y) {
  .slopesprime(
    RX = NULL,
    ryX = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Intercept \eqn{\beta_{1}}
#'
#' @description Derives the intercept \eqn{\beta_1} of a linear regression model
#'   from the \eqn{p \times 1} regression slopes
#'   \eqn{\left( \boldsymbol{\beta}_{2, \cdots, k} \right)},
#'   the mean of the regressand \eqn{\left( \mu_y \right)},
#'   and the \eqn{p \times 1} means of regressors \eqn{{X}_{2}, {X}_{3}, \dots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
#'
#' @details The intercept \eqn{\beta_1} is given by
#'   \deqn{
#'     \beta_1 = \mu_y - \boldsymbol{\mu}_{\mathbf{X}}
#'     \boldsymbol{\beta}_{2, \cdots, k}^{T} .
#'   }
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams Sigmatheta
#' @inheritParams mutheta
#' @inheritParams betahat
#' @param muy Numeric.
#'   Mean of the regressand variable \eqn{\left( \mu_{\mathbf{y}} \right)} .
#' @return Returns the intercept \eqn{\beta_1}
#'   of a linear regression model derived from the means
#'   and the slopes \eqn{\left( \boldsymbol{\beta}_{2, \cdots, k} \right)} .
#' @export
.intercept <- function(slopes = NULL,
                       muy = NULL,
                       muX = NULL,
                       X,
                       y) {
  if (is.null(slopes) | is.null(muy) | is.null(muX)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      plot = FALSE,
      moments = FALSE,
      cor = FALSE
    )
    SigmaX <- descriptives[["SigmaX"]]
    sigmayX <- descriptives[["sigmayX"]]
    mu <- descriptives[["mu"]]
    muy <- mu[1]
    muX <- mu[-1]
    slopes <- .slopes(
      SigmaX = SigmaX,
      sigmayX = sigmayX
    )
  }
  drop(
    muy - sum(crossprod(as.vector(muX), as.vector(slopes)))
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Intercept \eqn{\beta_{1}}
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams .intercept
#' @inherit .intercept description details return
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' intercept(X = X, y = y)
#' @export
intercept <- function(X,
                      y) {
  .intercept(
    slopes = NULL,
    muy = NULL,
    muX = NULL,
    X = X,
    y = y
  )
}

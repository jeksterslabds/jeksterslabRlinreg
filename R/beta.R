#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'
#' @description Derives the slopes \eqn{\boldsymbol{\beta}_{2, \cdots, k}}
#'   of a linear regression model (\eqn{\boldsymbol{\beta}} minus the intercept)
#'   as a function of covariances.
#'
#' @details The linear regression slopes are calculated using
#'   \deqn{
#'     \boldsymbol{\beta}_{2, \cdots, k} =
#'     \boldsymbol{\Sigma}_{\mathbf{X}}^{T} \boldsymbol{\sigma}_{\mathbf{y}, \mathbf{X}}
#'   }
#'
#'   where
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}}
#'     is the \eqn{p \times p} covariance matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k} and
#'   - \eqn{\boldsymbol{\sigma}_{\mathbf{y}, \mathbf{X}}}
#'     is the \eqn{p \times 1} column vector
#'     of the covariances between the regressand \eqn{y} variable
#'     and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams Sigmatheta
#' @inheritParams betahat
#' @param sigmayX Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} vector of covariances between the regressand \eqn{y} variable
#'   and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \boldsymbol{\sigma}_{\mathbf{y}, \mathbf{X}}
#'     = \left\{ \sigma_{y, X_2}, \sigma_{y, X_3}, \cdots, \sigma_{y, X_k} \right\}^{T} \right)}.
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
      cor = FALSE,
      mardia = FALSE
    )
    SigmaX <- descriptives[["SigmaXhat"]]
    sigmayX <- descriptives[["sigmayXhat"]]
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
#' @details The linear regression standardized slopes are calculated using
#'   \deqn{
#'     \boldsymbol{\beta}_{2, \cdots, k}^{\prime} =
#'     \mathbf{R}_{\mathbf{X}}^{T} \mathbf{r}_{\mathbf{y}, \mathbf{X}}
#'   }
#'
#'   where
#'   - \eqn{\mathbf{R}_{\mathbf{X}}}
#'     is the \eqn{p \times p} correlation matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k} and
#'   - \eqn{\mathbf{r}_{\mathbf{y}, \mathbf{X}}}
#'     is the \eqn{p \times 1} column vector
#'     of the correlations between the regressand \eqn{y} variable
#'     and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams betahat
#' @param RX `p` by `p` numeric matrix.
#'   \eqn{p \times p} matrix of correlations between the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \mathbf{R}_{\mathbf{X}} \right)}.
#' @param ryX Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} vector of correlations between the regressand variable \eqn{y} and the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \mathbf{r}_{\mathbf{y}, \mathbf{X}}
#'     = \left\{ r_{y, X_2}, r_{y, X_3}, \cdots, r_{y, X_k} \right\}^{T} \right)}.
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
      cor = FALSE,
      mardia = FALSE
    )
    RX <- descriptives[["RXhat"]]
    ryX <- descriptives[["ryXhat"]]
  }
  out <- solve(RX) %*% ryX
  colnames(out) <- "std.slopes"
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
      cor = FALSE,
      mardia = FALSE
    )
    SigmaX <- descriptives[["SigmaXhat"]]
    sigmayX <- descriptives[["sigmayXhat"]]
    muhat <- descriptives[["muhat"]]
    muy <- muhat[1]
    muX <- muhat[-1]
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

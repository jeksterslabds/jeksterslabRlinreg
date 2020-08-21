#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimates of Regression Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
#'
#' @details Estimates of the linear regression slopes are calculated using
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}_{2, \cdots, k} =
#'     \boldsymbol{\hat{\Sigma}}_{\mathbf{X}}^{T} \boldsymbol{\hat{\sigma}}_{\mathbf{y}, \mathbf{X}}
#'   }
#'
#'   where
#'   - \eqn{\boldsymbol{\hat{\Sigma}}_{\mathbf{X}}}
#'     is the \eqn{p \times p} covariance matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k} and
#'   - \eqn{\boldsymbol{\hat{\sigma}}_{\mathbf{y}, \mathbf{X}}}
#'     is the \eqn{p \times 1} column vector
#'     of the covariances between the regressand \eqn{y} variable
#'     and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahat
#' @param SigmaXhat `p` by `p` numeric matrix.
#'   \eqn{p \times p} matrix of estimated variances and covariances between regressor variables
#'   \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \boldsymbol{\hat{\Sigma}}_{\mathbf{X}} \right)}.
#' @param sigmayXhat Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} vector of estimated covariances between the regressand \eqn{y} variable
#'   and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \boldsymbol{\hat{\sigma}}_{\mathbf{y}, \mathbf{X}}
#'     = \left\{ \hat{\sigma}_{y, X_2}, \hat{\sigma}_{y, X_3}, \cdots, \hat{\sigma}_{y, X_k} \right\}^{T} \right)}.
#' @return Returns the estimated slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
#'   of a linear regression model derived from the estimated variance-covariance matrix.
#' @export
.slopeshat <- function(SigmaXhat = NULL,
                       sigmayXhat = NULL,
                       X,
                       y) {
  .slopes(
    SigmaX = SigmaXhat,
    sigmayX = sigmayXhat,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimates of Regression Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}}
#'
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams .slopeshat
#' @inherit .slopeshat details return
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopeshat(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' slopeshat(X = X, y = y)
#' @export
slopeshat <- function(X,
                      y) {
  .slopeshat(
    SigmaXhat = NULL,
    sigmayXhat = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimates of Regression Standardized Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}}
#'
#' @details Estimates of the linear regression standardized slopes are calculated using
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime} =
#'     \mathbf{\hat{R}}_{\mathbf{X}}^{T} \mathbf{\hat{r}}_{\mathbf{y}, \mathbf{X}}
#'   }
#'
#'   where
#'   - \eqn{\mathbf{\hat{R}}_{\mathbf{X}}}
#'     is the \eqn{p \times p} estimated correlation matrix of the regressor variables \eqn{X_2, X_3, \cdots, X_k} and
#'   - \eqn{\mathbf{\hat{r}}_{\mathbf{y}, \mathbf{X}}}
#'     is the \eqn{p \times 1} column vector
#'     of the estimated correlations between the regressand \eqn{y} variable
#'     and regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahat
#' @param RXhat `p` by `p` numeric matrix.
#'   \eqn{p \times p} matrix of estimates of correlations between the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \mathbf{\hat{R}}_{\mathbf{X}} \right)}.
#' @param ryXhat Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} vector of estimates of correlations between the regressand variable \eqn{y} and the regressor variables \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \mathbf{\hat{r}}_{\mathbf{y}, \mathbf{X}}
#'     = \left\{ \hat{r}_{y, X_2}, \hat{r}_{y, X_3}, \cdots, \hat{r}_{y, X_k} \right\}^{T} \right)}.
#' @return Returns the estimated standardized slopes
#'   \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}}
#'   of a linear regression model derived from the estimated correlation matrix.
#' @export
.slopeshatprime <- function(RXhat = NULL,
                            ryXhat = NULL,
                            X,
                            y) {
  .slopesprime(
    RX = RXhat,
    ryX = ryXhat,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimates of Regression Standardized Slopes \eqn{\boldsymbol{\hat{\beta}}_{2, \cdots, k}^{\prime}}
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams .slopeshatprime
#' @inherit .slopeshatprime details return
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' slopeshatprime(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' slopeshatprime(X = X, y = y)
#' @export
slopeshatprime <- function(X,
                           y) {
  .slopeshatprime(
    RXhat = NULL,
    ryXhat = NULL,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimated Regression Intercept \eqn{\hat{\beta}_{1}}
#'
#' @details The intercept \eqn{\beta_1} is given by
#'   \deqn{
#'     \hat{\beta}_1 = \hat{\mu}_y - \boldsymbol{\hat{\mu}}_{\mathbf{X}}
#'     \boldsymbol{\hat{\beta}}_{2, \cdots, k}^{T} .
#'   }
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams betahat
#' @param slopeshat Numeric vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} column vector of estimated regression slopes \eqn{\left( \boldsymbol{\hat{\beta}}_{2, 3, \cdots, k} = \left\{ \hat{\beta}_2, \hat{\beta}_3, \cdots, \hat{\beta}_k \right\}^{T} \right)} .
#' @param muyhat Numeirc.
#'   Estimated mean of the regressand variable \eqn{y}
#'   \eqn{\left( \hat{\mu}_y \right)}.
#' @param muXhat Vector of length `p` or `p` by `1` matrix.
#'   \eqn{p \times 1} column vector of the estimated means of the regressor variables
#'   \eqn{X_2, X_3, \cdots, X_k}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} = \left\{ \mu_{X_2}, \mu_{X_3}, \cdots, \mu_{X_k} \right\} \right)}.
#' @return Returns the estimated intercept \eqn{\hat{\beta}_1}
#'   of a linear regression model derived from the estimated means
#'   and the slopes \eqn{\left( \boldsymbol{\hat{\beta}}_{2, \cdots, k} \right)} .
#' @export
.intercepthat <- function(slopeshat = NULL,
                          muyhat = NULL,
                          muXhat = NULL,
                          X,
                          y) {
  .intercept(
    slopes = slopeshat,
    muy = muyhat,
    muX = muXhat,
    X = X,
    y = y
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Estimated Regression Intercept \eqn{\hat{\beta}_{1}}
#'
#' @family beta-hat functions
#' @keywords beta-hat-ols
#' @inheritParams .intercepthat
#' @inherit .intercepthat details return
#' @examples
#' # Simple regression------------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' X <- X[, c(1, ncol(X))]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' intercepthat(X = X, y = y)
#'
#' # Multiple regression----------------------------------------------
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' intercepthat(X = X, y = y)
#' @export
intercepthat <- function(X,
                         y) {
  .intercepthat(
    slopeshat = NULL,
    muyhat = NULL,
    muXhat = NULL,
    X = X,
    y = y
  )
}

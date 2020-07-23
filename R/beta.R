
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}}
#'
#' @description Derives the slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}} of a linear regression model
#'   (\eqn{\boldsymbol{\beta}} minus the intercept)
#'   as a function of covariances.
#'
#' @details The linear regression slopes is given by
#'   \deqn{
#'     \boldsymbol{\beta}_{2 \cdots k}
#'     =
#'     \mathrm{V}_{\mathbf{X}}^{T}
#'     \mathrm{v}_{\mathbf{yX}}
#'   }
#'
#'   where
#'   - \eqn{\boldsymbol{\Sigma}_{\mathbf{X}}}
#'     is the \eqn{p \times p} covariance matrix of the regressor variables and
#'   - \eqn{\boldsymbol{\sigma}_{\mathbf{yX}}}
#'     is the \eqn{p \times 1} column vector of the covariances between the regressand and the regressors.
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams Sigmatheta
#' @param sigmayX Vector or `p` by `1` matrix of covariances between the regressand and the regressors.
#'   \eqn{\left( \boldsymbol{\sigma}_{\mathbf{yX}} \right)}.
#' @return Returns the slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}}
#'   of a linear regression model derived from the variance-covariance matrix.
#' @export
slopes <- function(SigmaX,
                   sigmayX) {
  drop(
    solve(SigmaX) %*% sigmayX
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Standardized Slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}^{*}}
#'
#' @description Derives the standardized slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}^{*}} of a linear regression model
#'   as a function of correlations.
#'
#' @details The linear regression standardized slopes is given by
#'   \deqn{
#'     \boldsymbol{\beta}_{2 \cdots k}^{\prime}
#'     =
#'     \mathrm{R}_{\mathbf{X}}^{T}
#'     \mathrm{r}_{\mathbf{yX}}
#'   }
#'
#'   where
#'   - \eqn{\mathbf{R}_{\mathbf{X}}}
#'     is the \eqn{p \times p} correlation matrix of the regressor variables and
#'   - \eqn{\mathbf{r}_{\mathbf{yX}}}
#'     is the \eqn{p \times 1} column vector of the correlations between the regressand and the regressors.
#'
#' @family parameter functions
#' @keywords parameter
#' @param RX `p` by `p` numeric matrix.
#'   Correlations between the regressors.
#' @param ryX Vector or `p` by `1` matrix of correlations between the regressand and the regressors.
#' @return Returns the standardized slopes \eqn{\boldsymbol{\beta}_{2 \cdots k}^{*}}
#'   of a linear regression model derived from the correlation matrix.
#' @export
slopesprime <- function(RX,
                        ryX) {
  drop(
    solve(RX) %*% ryX
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Regression Intercept \eqn{\beta_{1}}
#'
#' @description Derives the intercept \eqn{\beta_1} of a linear regression model
#'   from the \eqn{p \times 1} regression slopes \eqn{\left( \boldsymbol{\beta}_{2 \cdots k} \right)},
#'   the mean of the regressand \eqn{\left( \mu_y \right)},
#'   and the \eqn{p \times 1} means of regressors \eqn{{X}_{2}, {X}_{3}, \dots, {X}_{k}}
#'   \eqn{\left( \boldsymbol{\mu}_{\mathbf{X}} \right)} .
#'
#' @details The intercept is given by
#'   \deqn{
#'     \beta_1
#'     =
#'     \mu_y
#'     -
#'     \boldsymbol{\mu}_{\mathbf{X}}
#'     \boldsymbol{\beta}_{2 \cdots k}^{T}
#'   }
#'
#' @family parameter functions
#' @keywords parameter
#' @inheritParams Sigmatheta
#' @inheritParams mutheta
#' @param muy Numeric.
#'   Mean of the regressand variable \eqn{\left( \mu_{\mathbf{y}} \right)} .
#' @return Returns the intercept \eqn{\beta_1}
#'   of a linear regression model derived from the means
#'   and the slopes \eqn{\left( \boldsymbol{\beta}_{2 \cdots k} \right)} .
#' @export
intercept <- function(slopes,
                      muy,
                      muX) {
  muy - sum(as.vector(muX) * t(as.vector(slopes)))
}

# Beta-hat from Summary Matrices
# Ivan Jacob Agaloos Pesigan

#' Beta-hat Slopes (\eqn{\boldsymbol{\hat{\beta}}_{\mathrm{slopes}}}) - Covariances
#'
#' Calculates the slopes of a linear regression model
#'   (\eqn{\boldsymbol{\hat{\beta}}} minus the intercept)
#'   as a function of covariances
#'   given by
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}_{\mathrm{slopes}}
#'     =
#'     \mathrm{V}_{\mathbf{X}}^{\prime}
#'     \mathrm{v}_{\mathbf{yX}}
#'   }
#'   where
#'   \eqn{\mathbf{V}_{\mathbf{X}}}
#'   is the covariance matrix
#'   of the regressor variables
#'   and
#'   \eqn{ \mathbf{v}_{\mathbf{yX}}}
#'   is the column vector
#'   of the covariances between
#'   the regressand
#'   and the regressors.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param VX Matrix.
#'   Covariances between the regressors.
#' @param vyX Vector
#'   or
#'   \eqn{\left( k - 1 \right) \times 1}
#'   matrix
#'   of covariances between the regressand
#'   and the regressors.
#' @return
#'   Returns the slopes
#'   (\eqn{\boldsymbol{\hat{\beta}}_\mathrm{slopes}})
#'   of a linear regression model
#'   derived from the variance-covariance matrix.
#' @family beta-hat from summary matrices functions
#' @export
betahat_slopes_cov <- function(VX,
                               vyX) {
  drop(
    solve(VX) %*% vyX
  )
}

#' Standardized Beta-hat Slopes (\eqn{\boldsymbol{\hat{\beta}}_{\mathrm{std}}}) - Correlations
#'
#' Calculates the standardized slopes of a linear regression model
#'   (\eqn{\boldsymbol{\hat{\beta}_{\mathrn{std}}}})
#'   as a function of correlations
#'   given by
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}_{\mathrm{std}}
#'     =
#'     \mathrm{R}_{\mathbf{X}}^{\prime}
#'     \mathrm{r}_{\mathbf{yX}}
#'   }
#' where
#' \eqn{\mathbf{R}_{\mathbf{X}}}
#' is the correlation matrix
#' of the regressor variables
#' and
#' \eqn{ \mathbf{r}_{\mathbf{yX}}}
#' is the column vector
#' of the correlations between
#' the regressand
#' and the regressors.
#' Note that the \eqn{y}-intercept
#' is zero when the variables are scaled.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param RX Matrix.
#'   Correlations between the regressors.
#' @param ryX Vector
#'   or
#'   \eqn{\left( k - 1 \right) \times 1}
#'   matrix
#'   of correlations between the regressand
#'   and the regressors.
#' @return
#'   Returns the standardized slopes
#'   (\eqn{\boldsymbol{\hat{\beta}}_\mathrm{std}})
#'   of a linear regression model
#'   derived from the correlation matrix.
#' @family beta-hat from summary matrices functions
#' @export
betahat_std_slopes_cor <- function(RX,
                                   ryX) {
  drop(
    solve(RX) %*% ryX
  )
}

#' Beta-hat Intercept (\eqn{\boldsymbol{\hat{\beta}}_{\mathrm{intercept}}})
#'
#' Calculates the intercept of a linear regression model
#'   (\eqn{\boldsymbol{\hat{\beta}}} minus the slopes)
#'   as a function of sample means and
#'   regression slopes
#'   given by
#'   \deqn{
#'     \boldsymbol{\hat{\beta}}_{\mathrm{intercept}}
#'     =
#'     \boldsymbol{\hat{\beta}}_{\mathrm{slopes}}^{\prime}
#'     \boldsymbol{\hat{\mu}}
#'   }
#' where
#' \eqn{\boldsymbol{\hat{\beta}}_{\mathrm{slopes}}}
#' is a vector of estimated regression slopes
#' with the last element as \eqn{0},
#' and
#' \eqn{\boldsymbol{\hat{\mu}}}
#' is a vector of sample means
#' of the regressor variables
#' and the regressand variable.
#'
#' @param betahat_slopes Vector.
#'   Estimates of unknown regression slopes.
#' @param muhatX Vector.
#'   Sample means of regressors.
#' @param muhaty Numeric.
#'   Sample mean of regressand.
#' @family beta-hat from summary matrices functions
#' @export
betahat_intercept <- function(betahat_slopes,
                              muhatX,
                              muhaty) {
  out <- as.vector(
    mean_str(
      beta_slopes = betahat_slopes,
      muX = muhatX,
      muy = muhaty
    )
  )
  out[1]
}

#' Beta-hat (\eqn{\boldsymbol{\hat{\beta}}}) - From Means and Covariance Matrix
#'
#' Calculates \eqn{\boldsymbol{\hat{\beta}}}
#' from means and covariance matrix.
#' See
#' [`.betahat_mat()`],
#' [`betahat_intercept()`],
#' [`betahat_slopes_cov()`].
#'
#' If
#' `VX`,
#' `vyX`,
#' `muhatX`,
#' or
#' `muhaty`
#' is `NULL`
#' these values are calculated
#' from
#' `X`
#' and
#' `y`.
#' `X` and `y` are ignored when
#' `VX`,
#' `vyX`,
#' `muhatX`,
#' and
#' `muhaty`
#' are provided.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param VX Matrix.
#'   Covariances between the regressors.
#' @param vyX Vector
#'   or
#'   \eqn{\left( k - 1 \right) \times 1}
#'   matrix
#'   of covariances between the regressand
#'   and the regressors.
#' @inheritParams betahat_intercept
#' @inheritParams betahat_inv
#' @importFrom stats cov
#' @family beta-hat from summary matrices functions
#' @export
.betahat_mat <- function(VX = NULL,
                         vyX = NULL,
                         muhatX = NULL,
                         muhaty = NULL,
                         X = NULL,
                         y = NULL) {
  if (is.null(VX) | is.null(vyX) | is.null(muhatX) | is.null(muhaty)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      muhat = TRUE
    )
    VX <- descriptives[["VX"]]
    vyX <- descriptives[["vyX"]]
    muhatX <- descriptives[["muhatX"]]
    muhaty <- descriptives[["muhaty"]]
  }
  betahat_slopes <- betahat_slopes_cov(
    VX = VX,
    vyX = vyX
  )
  betahat_slopes <- as.vector(betahat_slopes)
  betahat_intercept <- betahat_intercept(
    betahat_slopes = betahat_slopes,
    muhatX = muhatX,
    muhaty = muhaty
  )
  betahat_intercept <- as.vector(betahat_intercept)
  betahat <- c(
    betahat_intercept,
    betahat_slopes
  )
  matrix(
    data = betahat,
    ncol = 1
  )
}

#' Beta-hat (\eqn{\boldsymbol{\hat{\beta}}}) - From Means and Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .betahat_mat
#' @inherit .betahat_mat description
#' @family beta-hat from summary matrices functions
#' @export
betahat_mat <- function(X,
                        y) {
  .betahat_mat(
    VX = NULL,
    vyX = NULL,
    muhatX = NULL,
    muhaty = NULL,
    X = X,
    y = y
  )
}

#' Standardized Beta-hat Slopes (\eqn{\boldsymbol{\hat{\beta}}_{\mathrm{std}}}) - Correlations
#'
#' Calculates \eqn{\boldsymbol{\hat{\beta}}_{\mathrm{std}}}
#' from the correlation matrix.
#' See
#' [`betahat_std_slopes_cor()`].
#'
#' If
#' `RX`,
#' or
#' `ryX`,
#' is `NULL`
#' these values are calculated
#' from
#' `X`
#' and
#' `y`.
#' `X` and `y` are ignored when
#' `RX`,
#' and
#' `ryX`,
#' are provided.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams betahat_std_slopes_cor
#' @inheritParams betahat_inv
#' @inherit betahat_std_slopes_cor description return
#' @export
.betahat_std_mat <- function(RX = NULL,
                             ryX = NULL,
                             X = NULL,
                             y = NULL) {
  if (is.null(RX) | is.null(ryX)) {
    descriptives <- descriptives(
      X = X,
      y = y,
      mu = FALSE
    )
    RX <- descriptives[["RX"]]
    ryX <- descriptives[["ryX"]]
  }
  betahat_slopes <- betahat_std_slopes_cor(
    RX = RX,
    ryX = ryX
  )
  betahat <- c(
    0,
    betahat_slopes
  )
  matrix(
    data = betahat,
    ncol = 1
  )
}

#' Standardized Beta-hat Slopes (\eqn{\boldsymbol{\hat{\beta}}_{\mathrm{std}}}) - Correlations
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .betahat_std_mat
#' @inherit .betahat_std_mat description return
#' @export
betahat_std_mat <- function(X,
                            y) {
  .betahat_std_mat(
    RX = NULL,
    ryX = NULL,
    X = X,
    y = y
  )
}

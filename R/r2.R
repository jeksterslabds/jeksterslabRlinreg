#' R-square (from \eqn{RSS})
#'
#' Calculates the coefficient of determination using
#'   \deqn{
#'     R^2
#'     =
#'     1 - \frac{\textrm{Residual sum of squares}}{\textrm{Total sum of squares}}
#'   }
#'
#' If `rss = NULL`,
#' `rss` is computed
#' using [`ss_r()`]
#' with `X` and `y` as required arguments
#' and `beta_hat` as an optional argument.
#' If `tss = NULL`,
#' `tss` is computed
#' using [`ss_t()`]
#' with `y` as a required argument.
#' If `rss` and `tss` are provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param rss Numeric.
#'   Residual sum of squares.
#' @param tss Numeric.
#'   Total sum of squares.
#' @inheritParams ss_r
#' @return Returns the coefficient of determination \eqn{R^2}.
#' @family assessment of model quality functions
#' @inherit ss_r references
#' @export
r2_rss <- function(rss = NULL,
                   tss = NULL,
                   beta_hat = NULL,
                   X = NULL,
                   y = NULL) {
  if (is.null(rss)) {
    rss <- ss_r(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
  }
  if (is.null(tss)) {
    tss <- ss_t(
      y = y
    )
  }
  1 - (rss / tss)
}

#' R-square (from \eqn{ESS})
#'
#' Calculates the coefficient of determination using
#'   \deqn{
#'     R^2
#'     =
#'     \frac{\textrm{Explained sum of squares}}{\textrm{Total sum of squares}}.
#'   }
#'
#' If `ess = NULL`,
#' `ess` is computed
#' using [`ss_e()`]
#' with `X` and `y` as required arguments
#' and `beta_hat` as an optional argument.
#' If `tss = NULL`,
#' `tss` is computed
#' using [`ss_t()`]
#' with `y` as a required argument.
#' If `ess` and `tss` are provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param ess Numeric.
#'   Explained sum of squares.
#' @inheritParams r2_rss
#' @inherit r2_rss return references
#' @family assessment of model quality functions
#' @export
r2_ess <- function(ess = NULL,
                   tss = NULL,
                   beta_hat = NULL,
                   X = NULL,
                   y = NULL) {
  if (is.null(ess)) {
    ess <- ss_e(
      beta_hat = beta_hat,
      X = X,
      y = y
    )
  }
  if (is.null(tss)) {
    tss <- ss_t(
      y = y
    )
  }
  ess / tss
}

#' R-square
#'
#' Calculates the coefficient of determination using
#'   \deqn{
#'     R^2
#'     =
#'     1 - \frac{\textrm{Residual sum of squares}}{\textrm{Total sum of squares}}
#'   }
#' or
#'   \deqn{
#'     R^2
#'     =
#'     \frac{\textrm{Explained sum of squares}}{\textrm{Total sum of squares}}.
#'   }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param fromrss Logical.
#'   If `TRUE`,
#'   calculates the coefficient of determinism
#'   from `RSS`.
#'   If `FALSE`,
#'   calculates the coefficient of determinism
#'   from `ESS`.
#' @inheritParams r2_rss
#' @family assessment of model quality functions
#' @inherit r2_rss references
#' @export
r2 <- function(beta_hat = NULL,
               X,
               y,
               fromrss = TRUE) {
  if (fromrss) {
    return(
      r2_rss(
        rss = NULL,
        tss = NULL,
        beta_hat = beta_hat,
        X = X,
        y = y
      )
    )
  } else {
    return(
      r2_ess(
        ess = NULL,
        tss = NULL,
        beta_hat = beta_hat,
        X = X,
        y = y
      )
    )
  }
}

#' Adjusted R-square (from \eqn{R^2})
#'
#' Calculates the adjusted coefficient of determination
#'   \deqn{
#'     \bar{R}^{2}
#'     =
#'     1
#'     -
#'     \left(
#'       \frac{RSS / \left( n - k \right)}{TSS / \left(n - 1 \right)}
#'     \right)
#'     =
#'     1
#'     -
#'     \left(
#'       1 - R^2
#'     \right)
#'     \frac{n - 1}{n - k}.
#'   }
#'
#' If `r2 = NULL`,
#' `r2` is computed
#' using [`r2()`]
#' with `X` and `y` as required arguments
#' and `beta_hat` as an optional argument.
#' If `r2` is provided,
#' `beta_hat`, `X`, and `y`
#' are not needed.
#'
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors
#'   including a regressor
#'   whose value is 1 for each observation.
#' @author Ivan Jacob Agaloos Pesigan
#' @param r2 Numeric.
#'   Coefficient of determinims \eqn{R^2}.
#' @inheritParams r2
#' @return Returns the adjusted coefficient of determination \eqn{\bar{R}^{2}}.
#' @family assessment of model quality functions
#' @inherit r2_rss references
#' @export
rbar2_r2 <- function(r2 = NULL,
                     n,
                     k,
                     beta_hat = NULL,
                     X,
                     y,
                     fromrss = TRUE) {
  if (is.null(r2)) {
    r2 <- r2(
      beta_hat = beta_hat,
      X = X,
      y = y,
      fromrss = fromrss
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  1 - (1 - r2) * ((n - 1) / (n - k))
}

#' Adjusted R-square
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams rbar2_r2
#' @inherit rbar2_r2 description return references
#' @family assessment of model quality functions
#' @export
rbar2 <- function(beta_hat = NULL,
                  X,
                  y) {
  rbar2_r2(
    r2 = NULL,
    beta_hat = beta_hat,
    X = X,
    y = y,
    fromrss = TRUE
  )
}

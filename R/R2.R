#' R-square (from \eqn{RSS})
#'
#' Calculates the coefficient of determination using
#'   \deqn{
#'     R^2
#'     =
#'     1 - \frac{\textrm{Residual sum of squares}}{\textrm{Total sum of squares}}
#'   }
#'
#' If `RSS = NULL`,
#' `RSS` is computed
#' using [`RSS()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `TSS = NULL`,
#' `TSS` is computed
#' using [`TSS()`]
#' with `y` as a required argument.
#' If `RSS` and `TSS` are provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param RSS Numeric.
#'   Residual sum of squares.
#' @param TSS Numeric.
#'   Total sum of squares.
#' @inheritParams RSS
#' @return Returns the coefficient of determination \eqn{R^2}.
#' @family assessment of model quality functions
#' @inherit RSS references
#' @export
.R2_RSS <- function(RSS = NULL,
                    TSS = NULL,
                    X = NULL,
                    y = NULL,
                    betahat = NULL) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  if (is.null(TSS)) {
    TSS <- TSS(
      y = y
    )
  }
  1 - (RSS / TSS)
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
#' If `ESS = NULL`,
#' `ESS` is computed
#' using [`ESS()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `TSS = NULL`,
#' `TSS` is computed
#' using [`TSS()`]
#' with `y` as a required argument.
#' If `ESS` and `TSS` are provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param ESS Numeric.
#'   Explained sum of squares.
#' @inheritParams .R2_RSS
#' @inherit .R2_RSS return references
#' @family assessment of model quality functions
#' @export
.R2_ESS <- function(ESS = NULL,
                    TSS = NULL,
                    X = NULL,
                    y = NULL,
                    betahat = NULL) {
  if (is.null(ESS)) {
    ESS <- ESS(
      X = X,
      y = y,
      betahat = betahat
    )
  }
  if (is.null(TSS)) {
    TSS <- TSS(
      y = y
    )
  }
  ESS / TSS
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
#' @param fromRSS Logical.
#'   If `TRUE`,
#'   calculates the coefficient of determinism
#'   from `RSS`.
#'   If `FALSE`,
#'   calculates the coefficient of determinism
#'   from `ESS`.
#' @inheritParams .R2_RSS
#' @family assessment of model quality functions
#' @inherit .R2_RSS references
#' @export
R2 <- function(X,
               y,
               betahat = NULL,
               fromRSS = TRUE) {
  if (fromRSS) {
    return(
      .R2_RSS(
        RSS = NULL,
        TSS = NULL,
        X = X,
        y = y,
        betahat = betahat
      )
    )
  } else {
    return(
      .R2_ESS(
        ESS = NULL,
        TSS = NULL,
        X = X,
        y = y,
        betahat = betahat
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
#' If `R2 = NULL`,
#' `R2` is computed
#' using [`R2()`]
#' with `X` and `y` as required arguments
#' and `betahat` as an optional argument.
#' If `R2` is provided,
#' `betahat`, `X`, and `y`
#' are not needed.
#'
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors
#'   including a regressor
#'   whose value is 1 for each observation.
#' @author Ivan Jacob Agaloos Pesigan
#' @param R2 Numeric.
#'   Coefficient of determinims \eqn{R^2}.
#' @inheritParams R2
#' @return Returns the adjusted coefficient of determination \eqn{\bar{R}^{2}}.
#' @family assessment of model quality functions
#' @inherit .R2_RSS references
#' @export
.Rbar2 <- function(R2 = NULL,
                   n,
                   k,
                   X,
                   y,
                   betahat = NULL,
                   fromRSS = TRUE) {
  if (is.null(R2)) {
    R2 <- R2(
      X = X,
      y = y,
      betahat = betahat,
      fromRSS = fromRSS
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  1 - (1 - R2) * ((n - 1) / (n - k))
}

#' Adjusted R-square
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .Rbar2
#' @inherit .Rbar2 description return references
#' @family assessment of model quality functions
#' @export
Rbar2 <- function(X,
                  y,
                  betahat = NULL) {
  .Rbar2(
    R2 = NULL,
    X = X,
    y = y,
    betahat = betahat,
    fromRSS = TRUE
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Coefficient of Determination \eqn{R^2} (from \eqn{RSS})
#'
#' @description Calculates the coefficient of determination using
#'   \deqn{
#'     R^2 = 1 - \frac{\textrm{Residual sum of squares}}
#'     {\textrm{Total sum of squares}} .
#'   }
#'
#' @details If `RSS = NULL`, `RSS` is computed using [`RSS()`] with `X` and `y` as required arguments.
#'   If `TSS = NULL`, `TSS` is computed using [`TSS()`] with `y` as a required argument.
#'   If `RSS` and `TSS` are provided, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams RSS
#' @inherit RSS references
#' @param RSS Numeric.
#'   Residual sum of squares.
#' @param TSS Numeric.
#'   Total sum of squares.
#' @return Returns the coefficient of determination \eqn{R^2} .
#' @export
.R2fromRSS <- function(RSS = NULL,
                       TSS = NULL,
                       X,
                       y) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y
    )
  }
  if (is.null(TSS)) {
    TSS <- TSS(
      y = y
    )
  }
  1 - (RSS / TSS)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Coefficient of Determination \eqn{R^2} (from \eqn{ESS})
#'
#' @description Calculates the coefficient of determination using
#'   \deqn{
#'     R^2 = \frac{\textrm{Explained sum of squares}}
#'     {\textrm{Total sum of squares}} .
#'   }
#'
#' @details If `ESS = NULL`, `ESS` is computed using [`ESS()`] with `X` and `y` as required arguments.
#'   If `TSS = NULL`, `TSS` is computed using [`TSS()`] with `y` as a required argument.
#'   If `ESS` and `TSS` are provided, `yhat`, `ybar`, `betahat`, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .R2fromRSS
#' @inheritParams ESS
#' @inherit .R2fromRSS return references
#' @param ESS Numeric.
#'   Explained sum of squares.
#' @export
.R2fromESS <- function(ESS = NULL,
                       TSS = NULL,
                       X,
                       y) {
  if (is.null(ESS)) {
    ESS <- ESS(
      X = X,
      y = y
    )
  }
  if (is.null(TSS)) {
    TSS <- TSS(
      y = y
    )
  }
  ESS / TSS
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Coefficient of Determination \eqn{R^2}
#'
#' @description Calculates the coefficient of determination using
#'   \deqn{
#'     R^2 = 1 - \frac{\textrm{Residual sum of squares}}
#'     {\textrm{Total sum of squares}}
#'   }
#'   or
#'   \deqn{
#'     R^2 = \frac{\textrm{Explained sum of squares}}
#'     {\textrm{Total sum of squares}} .
#' }
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .R2fromRSS
#' @inherit .R2fromRSS references
#' @param fromRSS Logical.
#'   If `TRUE`, calculates the coefficient of determination from `RSS`.
#'   If `FALSE`, calculates the coefficient of determination from `ESS`.
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' R2(
#'   X = X,
#'   y = y
#' )
#' @export
R2 <- function(X,
               y,
               fromRSS = TRUE) {
  if (fromRSS) {
    return(
      .R2fromRSS(
        RSS = NULL,
        TSS = NULL,
        X = X,
        y = y
      )
    )
  } else {
    return(
      .R2fromESS(
        ESS = NULL,
        TSS = NULL,
        X = X,
        y = y
      )
    )
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Adjusted R-squared \eqn{\bar{R}^{2}} (from \eqn{R^2})
#'
#' @description Calculates the adjusted coefficient of determination
#'   \deqn{
#'     \bar{R}^{2} = 1 - \left(\frac{RSS / \left( n - k \right)}
#'     {TSS / \left( n - 1 \right)} \right)
#'     = 1 - \left( 1 - R^2 \right) \frac{n - 1}{n - k} .
#'   }
#'
#' @details If `R2 = NULL`, `R2` is computed using [`R2()`]
#'   with `X` and `y` as required arguments.
#'   If `R2` is provided, `X`, and `y` are not needed.
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams R2
#' @inherit R2 references
#' @param n Integer.
#'   Sample size.
#' @param k Integer.
#'   Number of regressors including a regressor whose value is 1 for each observation.
#' @param R2 Numeric.
#'   Coefficient of determination \eqn{R^2} .
#' @return Returns the adjusted coefficient of determination \eqn{\bar{R}^{2}} .
#' @export
.Rbar2 <- function(R2 = NULL,
                   n,
                   k,
                   X,
                   y,
                   fromRSS = TRUE) {
  if (is.null(R2)) {
    R2 <- R2(
      X = X,
      y = y,
      fromRSS = fromRSS
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  1 - (1 - R2) * ((n - 1) / (n - k))
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Adjusted R-squared \eqn{\bar{R}^{2}}
#'
#' @family assessment of model quality functions
#' @keywords model-assessment
#' @inheritParams .Rbar2
#' @inherit .Rbar2 description return references
#' @examples
#' X <- jeksterslabRdatarepo::wages.matrix[["X"]]
#' # age is removed
#' X <- X[, -ncol(X)]
#' y <- jeksterslabRdatarepo::wages.matrix[["y"]]
#' Rbar2(
#'   X = X,
#'   y = y
#' )
#' @export
Rbar2 <- function(X,
                  y) {
  .Rbar2(
    R2 = NULL,
    X = X,
    y = y,
    fromRSS = TRUE
  )
}

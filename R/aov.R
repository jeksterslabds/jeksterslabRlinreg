#' F-Test
#' (From
#' \eqn{
#'   R^2
#' }
#' )
#'
#' Calculates the
#' \eqn{
#'   F
#' }
#' statistic from
#' \eqn{
#'   R^2
#' }
#' using
#' \deqn{
#'   F
#'   =
#'   \frac{
#'     \frac{
#'       R^2
#'     }
#'     {
#'       k
#'       -
#'       1
#'     }
#'     }
#'     {
#'     \frac{
#'       1
#'       -
#'       R^2
#'     }
#'     {
#'       n
#'       -
#'       k
#'     }
#'   }
#' } .
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .Rbar2
#' @return Returns a vector with the following elements
#' \describe{
#'   \item{R2}{
#'     Coefficient of determination
#'     \eqn{
#'       \left(
#'         R^2
#'       \right)
#'     } .
#'   }
#'   \item{F}{
#'     F Statistic
#'     \eqn{
#'       \left(
#'         F
#'       \right)
#'     } .
#'   }
#'   \item{p}{p value
#'     \eqn{
#'       \left(
#'         p
#'       \right)
#'     } .
#'   }
#'   \item{df1}{
#'     Degrees of Freedom - Model
#'     \eqn{
#'       \left(
#'         df_1
#'         =
#'         k
#'         -
#'         1
#'       \right)
#'     } .
#'   }
#'   \item{df2}{
#'     Degrees of Freedom - Error
#'     \eqn{
#'       \left(
#'         df_2
#'         =
#'         n
#'         -
#'         k
#'       \right)
#'     } .
#'   }
#' }
#' @importFrom stats pf
#' @export
.F_R2 <- function(R2 = NULL,
                  n,
                  k,
                  betahat = NULL,
                  X = X,
                  y = y) {
  if (is.null(R2)) {
    R2(
      X = X,
      y = y,
      betahat = betahat,
      fromRSS = TRUE
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  df1 <- k - 1
  df2 <- n - k
  F <- (R2 / (k - 1)) / ((1 - R2) / (n - k))
  p <- pf(
    q = F,
    df1 = df1,
    df2 = df2,
    lower.tail = FALSE
  )
  c(
    R2 = R2,
    F = F,
    p = p,
    df1 = df1,
    df2 = df2
  )
}

#' F-Test
#' (From
#' \eqn{
#'   R^2
#' }
#' )
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .F_R2
F_R2 <- function(X,
                 y,
                 betahat = NULL) {
  .F_R2(
    R2 = NULL,
    X = X,
    y = y,
    betahat = NULL
  )
}

#' F-Test
#' \eqn{
#'   F_{
#'     m,
#'     \left(
#'       n
#'       -
#'       k
#'     \right)
#'   }
#' }
#'
#' Tests the incremental contribution of regressors
#' using
#' \deqn{
#'   F
#'   =
#'   \frac{
#'     \frac{
#'       RSS_R
#'       RSS_{
#'         UR
#'       }
#'     }
#'     {
#'       m
#'     }
#'   }
#'   {
#'     \frac{
#'       RSS_{
#'         UR
#'       }
#'     }
#'     {
#'       n
#'       -
#'       k
#'     }
#'   }
#'   \sim
#'   F_{
#'     m,
#'     \left(
#'       n
#'       -
#'       k
#'     \right)
#'   } .
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .F_R2
#' @param RSS_R Numeric.
#' Residual sum of squares from the restricted regression.
#' @param RSS_UR Numeric.
#' Residual sum of squares from the unrestricted regression.
#' @param m Integer.
#' Number of linear restrictions.
#' @param X_r Vector.
#' Vector of column indices used to subset X
#' for the restricted regression.
#' @export
.F_inc <- function(RSS_R = NULL,
                   RSS_UR = NULL,
                   n,
                   k,
                   m,
                   X,
                   y,
                   X_r) {
  if (is.null(RSS_UR)) {
    RSS_UR <- RSS(
      betahat = NULL,
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (is.null(RSS_R)) {
    X_r <- X[, X_r]
    X_r <- cbind(
      1,
      X_r
    )
    RSS_R <- RSS(
      betahat = NULL,
      X = X_r,
      y = y
    )
    n <- nrow(X_r)
    m <- k - ncol(X_r)
  }
  ((RSS_R - RSS_UR) / m) / (RSS_UR / (n - k))
}

#' F-Test
#' \eqn{
#'   F_{
#'     m,
#'     \left(
#'       n
#'       -
#'       k
#'     \right)
#'   }
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .F_inc
#' @inherit .F_inc description return references
#' @export
F_inc <- function(X,
                  y,
                  X_r) {
  .F_inc(
    RSS_R = NULL,
    RSS_UR = NULL,
    X = X,
    y = y,
    X_r = X_r
  )
}

#' Analysis of Variance
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams .R2_RSS
#' @inheritParams .R2_ESS
#' @inheritParams .Rbar2
#' @export
.aov <- function(RSS = NULL,
                 ESS = NULL,
                 n,
                 k,
                 X = X,
                 y = y,
                 betahat = NULL) {
  if (is.null(RSS)) {
    RSS(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (is.null(ESS)) {
    ESS(
      betahat = betahat,
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  TSS <- RSS + ESS
  df1 <- k - 1
  df2 <- n - k
  MSmodel <- ESS / df1
  MSerror <- RSS / df2
  F <- MSmodel / MSerror
  p <- pf(
    q = F,
    df1 = df1,
    df2 = df2,
    lower.tail = FALSE
  )
  df <- c(
    df1,
    df2,
    df1 + df2
  )
  SS <- c(
    ESS,
    RSS,
    TSS
  )
  MS <- c(
    MSmodel,
    MSerror,
    NA
  )
  F <- c(
    F,
    NA,
    NA
  )
  p <- c(
    p,
    NA,
    NA
  )
  out <- cbind(
    df,
    SS,
    MS,
    F,
    p
  )
  colnames(out) <- c(
    "df",
    "SS",
    "MS",
    "F",
    "p"
  )
  rownames(out) <- c(
    "Model",
    "Error",
    "Total"
  )
  out
}

#' Analysis of Variance
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inherit .aov
#' @export
aov <- function(X = X,
                y = y,
                betahat = NULL) {
  .aov(
    RSS = NULL,
    ESS = NULL,
    betahat = betahat,
    X = X,
    y = y
  )
}

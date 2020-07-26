#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Analysis of Variance (from \eqn{RSS} and \eqn{ESS})
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family hypothesis testing functions
#' @keywords inference
#' @inheritParams RSS
#' @inheritParams ESS
#' @inheritParams .R2fromRSS
#' @inheritParams .R2fromESS
#' @inheritParams .Rbar2
#' @importFrom stats pf
#' @export
.anovatable <- function(RSS = NULL,
                        ESS = NULL,
                        n,
                        k,
                        X,
                        y) {
  if (is.null(RSS)) {
    RSS <- RSS(
      X = X,
      y = y
    )
    n <- nrow(X)
    k <- ncol(X)
  }
  if (is.null(ESS)) {
    ESS <- ESS(
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Analysis of Variance
#'
#' @family hypothesis testing functions
#' @keywords inference
#' @inherit .anovatable
#' @export
anovatable <- function(X,
                       y) {
  .anovatable(
    RSS = NULL,
    ESS = NULL,
    X = X,
    y = y
  )
}

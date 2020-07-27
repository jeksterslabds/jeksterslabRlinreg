#' ---
#' title: "Tests: The Linear Regression Model (ANOVA Table)"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Tests: The Linear Regression Model (ANOVA Table)}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ include = FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#'
# The Linear Regression Model: ANOVA Table {#linreg-estimation-anovatable-example}
#'
#+ echo = FALSE
library(testthat)
library(jeksterslabRlinreg)
#'
#' ## Data
#'
#' See `jeksterslabRdatarepo::wages()` for the data set used in this example.
#'
#+
X <- jeksterslabRdatarepo::wages.matrix[["X"]]
X <- X[, -ncol(X)]
y <- jeksterslabRdatarepo::wages.matrix[["y"]]
head(X)
head(y)
#'
#' ## ANOVA Table
#'
#+
n <- nrow(X)
k <- ncol(X)
RSS <- RSS(
  X = X,
  y = y
)
ESS <- ESS(
  X = X,
  y = y
)
result1 <- .anovatable(
  RSS = RSS,
  ESS = ESS,
  n = n,
  k = k,
  X = NULL,
  y = NULL
)
result2 <- .anovatable(
  RSS = NULL,
  ESS = ESS,
  n = n,
  k = k,
  X = X,
  y = y
)
result3 <- .anovatable(
  RSS = RSS,
  ESS = NULL,
  n = n,
  k = k,
  X = X,
  y = y
)
result4 <- .anovatable(
  RSS = NULL,
  ESS = NULL,
  n = n,
  k = k,
  X = X,
  y = y
)
result5 <- anovatable(
  X = X,
  y = y
)
#'
#' ## `lm()` function
#'
#+
lmobj <- lm(
  wages ~ gender + race + union + education + experience,
  data = jeksterslabRdatarepo::wages
)
lm_anova <- anova(lmobj)
lm_RSS <- lm_anova["Residuals", "Sum Sq"]
lm_TSS <- sum(lm_anova[["Sum Sq"]])
lm_ESS <- lm_TSS - lm_RSS
lm_F <- summary(lmobj)$fstatistic
lm_df1 <- lm_F[[2]]
lm_df2 <- lm_F[[3]]
lm_df3 <- lm_df1 + lm_df2
lm_F <- lm_F[[1]]
lm_p <- pf(lm_F, df1 = lm_df1, df2 = lm_df2, lower.tail = FALSE)
lm_MS_model <- lm_ESS / lm_df1
lm_MS_error <- lm_RSS / lm_df2
#'
#'
#+
result_df1 <- c(
  result1["Model", "df"],
  result2["Model", "df"],
  result3["Model", "df"],
  result4["Model", "df"],
  result5["Model", "df"]
)
result_df2 <- c(
  result1["Error", "df"],
  result2["Error", "df"],
  result3["Error", "df"],
  result4["Error", "df"],
  result5["Error", "df"]
)
result_df3 <- c(
  result1["Total", "df"],
  result2["Total", "df"],
  result3["Total", "df"],
  result4["Total", "df"],
  result5["Total", "df"]
)
result_ss1 <- c(
  result1["Model", "SS"],
  result2["Model", "SS"],
  result3["Model", "SS"],
  result4["Model", "SS"],
  result5["Model", "SS"]
)
result_ss2 <- c(
  result1["Error", "SS"],
  result2["Error", "SS"],
  result3["Error", "SS"],
  result4["Error", "SS"],
  result5["Error", "SS"]
)
result_ss3 <- c(
  result1["Total", "SS"],
  result2["Total", "SS"],
  result3["Total", "SS"],
  result4["Total", "SS"],
  result5["Total", "SS"]
)
result_ms1 <- c(
  result1["Model", "MS"],
  result2["Model", "MS"],
  result3["Model", "MS"],
  result4["Model", "MS"],
  result5["Model", "MS"]
)
result_ms2 <- c(
  result1["Error", "MS"],
  result2["Error", "MS"],
  result3["Error", "MS"],
  result4["Error", "MS"],
  result5["Error", "MS"]
)
result_f <- c(
  result1["Model", "F"],
  result2["Model", "F"],
  result3["Model", "F"],
  result4["Model", "F"],
  result5["Model", "F"]
)
result_p <- c(
  result1["Model", "p"],
  result2["Model", "p"],
  result3["Model", "p"],
  result4["Model", "p"],
  result5["Model", "p"]
)
context("Test linreg-estimation-anovatable.")
test_that("df1", {
  for (i in seq_along(result_df1)) {
    expect_equivalent(
      lm_df1,
      result_df1[i]
    )
  }
})
test_that("df2", {
  for (i in seq_along(result_df2)) {
    expect_equivalent(
      lm_df2,
      result_df2[i]
    )
  }
})
test_that("df3", {
  for (i in seq_along(result_df3)) {
    expect_equivalent(
      lm_df3,
      result_df3[i]
    )
  }
})
test_that("ss1", {
  for (i in seq_along(result_ss1)) {
    expect_equivalent(
      lm_ESS,
      result_ss1[i]
    )
  }
})
test_that("ss2", {
  for (i in seq_along(result_ss2)) {
    expect_equivalent(
      lm_RSS,
      result_ss2[i]
    )
  }
})
test_that("ss3", {
  for (i in seq_along(result_ss3)) {
    expect_equivalent(
      lm_TSS,
      result_ss3[i]
    )
  }
})
test_that("ms1", {
  for (i in seq_along(result_ms1)) {
    expect_equivalent(
      lm_MS_model,
      result_ms1[i]
    )
  }
})
test_that("ms2", {
  for (i in seq_along(result_ms2)) {
    expect_equivalent(
      lm_MS_error,
      result_ms2[i]
    )
  }
})
test_that("f", {
  for (i in seq_along(result_f)) {
    expect_equivalent(
      lm_F,
      result_f[i]
    )
  }
})
test_that("p", {
  for (i in seq_along(result_p)) {
    expect_equivalent(
      lm_p,
      result_p[i]
    )
  }
})

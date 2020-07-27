#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Plots
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams .yminusyhat
#' @inheritParams .RSS
#' @inheritParams .tepsilonhat
#' @param tepsilonhat
#'   Studentized residuals.
#' @export
residual.plot <- function(yhat,
                          tepsilonhat,
                          epsilonhat,
                          h) {
  usr <- par("usr")
  on.exit(par(usr))
  par(mfrow = c(2, 2))
  lw1 <- suppressWarnings(loess(epsilonhat ~ yhat))
  j <- order(yhat)
  plot(
    x = yhat,
    y = epsilonhat,
    xlab = "Fitted",
    ylab = "Residuals",
    main = "Residuals vs. Fitted Values"
  )
  lines(yhat[j], lw1$fitted[j], col = "red")
  # abline(
  #  h = 0,
  #  col = "red"
  # )
  qqnorm(
    epsilonhat,
    main = "Normal Q-Q Plot of Residuals"
  )
  qqline(
    epsilonhat,
    col = "red"
  )
  sqrtabsstd <- sqrt(abs(tepsilonhat))
  lw2 <- suppressWarnings(loess(sqrtabsstd ~ yhat))
  j <- order(yhat)
  plot(
    x = yhat,
    y = sqrtabsstd,
    xlab = "Fitted",
    ylab = expression(sqrt(abs(Std. ~ Residuals))),
    main = "Scale-Location"
  )
  lines(yhat[j], lw2$fitted[j], col = "red")
  lw3 <- suppressWarnings(loess(tepsilonhat ~ h))
  j <- order(h)
  plot(
    x = h,
    y = tepsilonhat,
    xlab = "Leverage",
    ylab = "Std. Residuals",
    main = "Residuals vs. Leverage"
  )
  lines(h[j], lw2$fitted[j], col = "red")
}

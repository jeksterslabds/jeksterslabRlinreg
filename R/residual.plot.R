#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Residual Plots
#'
#' @family plotting functions
#' @keywords plot
#' @inheritParams .yminusyhat
#' @inheritParams .RSS
#' @param tepsilonhat
#'   Studentized residuals.
#' @export
residual.plot <- function(yhat,
                          tepsilonhat,
                          epsilonhat) {
  usr <- par("usr")
  on.exit(par(usr))
  par(mfrow = c(1, 2))
  plot(
    x = yhat,
    y = tepsilonhat,
    xlab = "Fitted",
    ylab = "t Residuals",
    main = "t Residuals vs. Fitted Values"
  )
  abline(
    h = 0,
    col = "red"
  )
  qqnorm(
    epsilonhat,
    main = "Residual Normal Q-Q Plot"
  )
  qqline(
    epsilonhat,
    col = "red"
  )
}

#' @title Scatter Plot Matrix
#'
#' @description Adapted
#'   from <http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs>
#'
#' @family plotting functions
#' @keywords plot
#' @import graphics
#' @importFrom stats loess cor.test
#' @param data Matrix or data frame.
#' @export
scatter.plot <- function(data) {
  panel.hist <- function(x, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y / max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
  }
  panel.cor <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits = 2)
    p <- cor.test(x, y)
    r.p <- round(p$p.value, digits = 3)
    txt <- paste0("r = ", r, "\np = ", r.p)
    text(0.5, 0.5, txt)
  }
  # Customize upper panel
  upper.panel <- function(x, y) {
    lw1 <- suppressWarnings(loess(y ~ x))
    j <- order(x)
    points(x, y)
    lines(x[j], lw1$fitted[j], col = "red")
  }
  # Create the plots
  pairs(
    data,
    lower.panel = panel.cor,
    upper.panel = upper.panel,
    diag.panel = panel.hist
  )
}

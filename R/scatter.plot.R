#' @title Scatter Plot Matrix
#'
#' @description Adapted
#'   from <http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs>
#'
#' @family plotting functions
#' @keywords plot
#' @import graphics
#' @param data Matrix or data frame.
#' @export
scatter.plot <- function(data) {
  panel.cor <- function(x, y) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits = 2)
    txt <- paste0("r = ", r)
    text(0.5, 0.5, txt)
  }
  # Customize upper panel
  upper.panel <- function(x, y) {
    points(x, y)
  }
  # Create the plots
  pairs(
    data,
    lower.panel = panel.cor,
    upper.panel = upper.panel
  )
}

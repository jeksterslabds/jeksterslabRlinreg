#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Studentized Residuals
#'
#' @description Calculates studentized residuals using
#'   \deqn{
#'     t_i = \frac{\hat{\varepsilon}_{i}}{\hat{\sigma}_{\varepsilon}^{2} \sqrt{1 - h_{ii}}}
#'   }
#'
#' @details If `epsilonhat`, `h`, or `sigma2epsilonhat` are `NULL`,
#'   they are calculated using `X` and `y`.
#'
#' @family residuals functions
#' @keywords residuals
#' @inheritParams .RSS
#' @inheritParams .vcovbetahat
#' @inherit .h references
#' @param h Numeric vector.
#'   Leverage.
#' @return Returns studentized residuals.
#' @export
.tepsilonhat <- function(epsilonhat = NULL,
                         h = NULL,
                         sigma2epsilonhat = NULL,
                         X = NULL,
                         y = NULL) {
  if (is.null(epsilonhat)) {
    epsilonhat <- epsilonhat(
      X = X,
      y = y
    )
  }
  if (is.null(h)) {
    h <- h(X)
  }
  if (is.null(sigma2epsilonhat)) {
    sigma2epsilonhat <- sigma2epsilonhat(
      X = X,
      y = y
    )
  }
  epsilonhat / sqrt(sigma2epsilonhat * (1 - h))
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Studentized Residuals
#' @family residuals functions
#' @keywords residuals
#' @inherit .tepsilonhat description return
#' @inheritParams .tepsilonhat
#' @export
tepsilonhat <- function(X,
                        y) {
  .tepsilonhat(
    epsilonhat = NULL,
    h = NULL,
    sigma2epsilonhat = NULL,
    X = X,
    y = y
  )
}

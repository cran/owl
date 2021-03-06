#' Obtain coefficients
#'
#' This function returns coefficients from a model fit by [owl()].
#'
#' If `exact == FALSE` and `sigma` is not in `object`,
#' then the returned coefficients will be approximated by linear interpolation.
#' If coefficients from another type of penalty sequence
#' (with a different `lambda`) are required, however,
#' please use [owl()] to refit the model.
#'
#' @param object an object of class `'Owl'`.
#' @param ... arguments that are passed on to [stats::update()] (and therefore
#'   also to [owl()]) if `exact = TRUE` and the given penalty
#'   is not in `object`
#' @inheritParams predict.Owl
#'
#' @return Coefficients from the model.
#'
#' @export
#' @examples
#' fit <- owl(mtcars$mpg, mtcars$vs, n_sigma = 1)
#' coef(fit)
coef.Owl <- function(object,
                     sigma = NULL,
                     exact = FALSE,
                     simplify = TRUE,
                     ...) {
  beta <- object$coefficients

  n_penalties <- dim(beta)[3]

  penalty <- object$sigma
  value <- sigma

  if (is.null(value)) {
    n_penalties <- length(penalty)
  } else if (all(value %in% penalty)) {
    n_penalties <- length(value)
    beta <- beta[, , penalty %in% value, drop = FALSE]
  } else if (exact) {
    object <- stats::update(object, sigma = sigma, ...)
    beta <- object$coefficients
  } else {
    stopifnot(value >= 0)
    interpolation_list <- interpolatePenalty(penalty, value)
    beta <- interpolateCoefficients(beta, interpolation_list)
    n_penalties <- length(value)
  }

  if (simplify)
    beta <- drop(beta)

  beta
}

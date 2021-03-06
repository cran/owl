#' Generate predictions from owl models
#'
#' Return predictions from models fit by [owl()].
#'
#' @param object an object of class `"owl"`, typically the result of
#'   a call to [owl()]
#' @param x new data
#' @param type type of prediction; `"link"` returns the linear predictors,
#'   `"response"` returns the result of applying the link function,
#'    and `"class"` returns class predictions.
#' @param ... ignored and only here for method consistency
#' @param sigma penalty parameter for SLOPE models; if `NULL`, the
#'   values used in the original fit will be used
#' @param simplify if `TRUE`, [base::drop()] will be called before returning
#'   the coefficients to drop extraneous dimensions
#' @param exact if `TRUE` and the given parameter values differ from those in
#'   the original fit, the model will be refit by calling [stats::update()] on
#'   the object with the new parameters. If `FALSE`, the predicted values
#'   will be based on interpolated coefficients from the original
#'   penalty path.
#'
#' @seealso [stats::predict()], [stats::predict.glm()]
#'
#' @return Predictions from the model with scale determined by `type`.
#'
#'
#' @examples
#' fit <- with(mtcars, owl(cbind(mpg, hp), vs, family = "binomial"))
#' predict(fit, with(mtcars, cbind(mpg, hp)), type = "class")
#'
#' @export
predict.Owl <- function(object,
                        x,
                        sigma = NULL,
                        type = "link",
                        simplify = TRUE,
                        ...) {
  # This method (the base method) only generates linear predictors

  if (inherits(x, "sparseMatrix"))
    x <- methods::as(x, "dgCMatrix")

  if (inherits(x, "data.frame"))
    x <- as.matrix(x)

  beta <- stats::coef(object, sigma = sigma, simplify = FALSE)

  intercept <- "(Intercept)" %in% dimnames(beta)[[1]]

  if (intercept)
    x <- methods::cbind2(1, x)

  n <- NROW(x)
  p <- NROW(beta)
  m <- NCOL(beta)
  n_penalties <- dim(beta)[3]

  stopifnot(p == NCOL(x))

  lin_pred <- array(dim = c(n, m, n_penalties),
                    dimnames = list(rownames(x),
                                    dimnames(beta)[[2]],
                                    dimnames(beta)[[3]]))

  for (i in seq_len(n_penalties))
    lin_pred[, , i] <- as.matrix(x %*% beta[, , i])

  lin_pred
}

#' @rdname predict.Owl
#' @export
predict.OwlGaussian <- function(object,
                                x,
                                sigma = NULL,
                                type = c("link", "response"),
                                simplify = TRUE,
                                ...) {
  type <- match.arg(type)

  out <- NextMethod(object, type = type) # always linear predictors

  if (simplify)
    out <- drop(out)

  out
}

#' @rdname predict.Owl
#' @export
predict.OwlBinomial <- function(object,
                                x,
                                sigma = NULL,
                                type = c("link", "response", "class"),
                                simplify = TRUE,
                                ...) {

  type <- match.arg(type)

  lin_pred <- NextMethod(object, type = type)

  out <- switch(
    type,
    link = lin_pred,
    response = 1 / (1 + exp(-lin_pred)),
    class = {
      cnum <- ifelse(lin_pred > 0, 2, 1)
      clet <- object$class_names[cnum]

      if (is.matrix(cnum))
        clet <- array(clet, dim(cnum), dimnames(cnum))

      clet
    }
  )

  if (simplify)
    out <- drop(out)

  out
}

#' @rdname predict.Owl
#' @export
predict.OwlPoisson <- function(object,
                               x,
                               sigma = NULL,
                               type = c("link", "response"),
                               exact = FALSE,
                               simplify = TRUE,
                               ...) {

  type <- match.arg(type)

  lin_pred <- NextMethod(object, type = type)

  out <- switch(
    type,
    link = lin_pred,
    response = exp(lin_pred)
  )

  if (simplify)
    out <- drop(out)

  out
}

#' @export
#' @rdname predict.Owl
predict.OwlMultinomial <- function(object,
                                   x,
                                   sigma = NULL,
                                   type = c("link", "response", "class"),
                                   exact = FALSE,
                                   simplify = TRUE,
                                   ...) {
  type <- match.arg(type)

  lin_pred <- NextMethod(object, type = type, simplify = FALSE)
  m <- NCOL(lin_pred)

  out <- switch(
    type,

    response = {
      n <- nrow(lin_pred)
      m <- ncol(lin_pred)
      n_sigma <- dim(lin_pred)[3]

      tmp <- array(0, c(n, m + 1, n_sigma))
      tmp[, 1:m, ] <- lin_pred

      aperm(apply(tmp, c(1, 3), function(x) exp(x)/sum(exp(x))), c(2, 1, 3))
    },

    link = lin_pred,

    class = {
      response <- stats::predict(object, x, type = "response", simplify = FALSE)
      tmp <- apply(response, c(1, 3), which.max)
      class_names <- object$class_names

      predicted_classes <-
        apply(tmp,
              2,
              function(a) factor(a, levels = 1:(m+1), labels = class_names))
      colnames(predicted_classes) <- dimnames(lin_pred)[[3]]
      predicted_classes
    }
  )

  if (simplify)
    out <- drop(out)

  out
}


#' Unscale a vector that was originally scaled using 'scale' function
#'
#' \code{unscale} takes a given scaled vector \code{x} that has been centered and scaled
#' (assumed using the \code{scale} function) and reverses the centering and scaling, if any.
#'
#' @param x A numeric vector that has been scaled and centered using the \code{scale} function.
#'
#' @return The original vector before any scaling and centering.
#'
#' @details This function assumes that the input vector \code{x} was previously scaled
#' using the \code{scale} function in R. It retrieves the mean (\code{mu}) and standard deviation
#' (\code{sigma}) used for scaling and centering, then reverses centering and scaling operations to obtain
#' the original vector.
#'
#' @examples
#' set.seed(123)
#' x = rnorm(10)
#' scaled_x = scale(x)
#' original_x = unscale(scaled_x)
#' all.equal(original_x, x)  # Should return TRUE
#'
#' @export
#'
unscale = function(x) {
  #"unscale" takes a given scaled vector x that has been centered and scaled
  #(assumed using scale function) and reverses the centering and scaling, if any.

  # First, confirm that x is a numeric vector
  if (!is.numeric(x)) {
    stop("Input must be numerical vector.")
  }

  # Assumed scaled using scale function in R, get mean and sd
  mu = attr(x, "scaled:center")
  sigma = attr(x, "scaled:scale")

  if (is.null(mu) || is.null(sigma)) {
    stop("Input vector x does not appear to have been scaled with function 'scale'.")
  }

  # Remove any centering and scaling
  x_unscaled = (x * sigma) + mu
  x_unscaled = as.matrix(x_unscaled)
  attributes(x_unscaled) = NULL #remove attributes

  return(x_unscaled)
}

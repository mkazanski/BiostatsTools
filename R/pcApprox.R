#' Perform principal component analysis and return an approximation of the data given a speecified number of PCs
#'
#' This function performs principal component analysis (PCA) on input data \code{x},
#' then for a specified number of principal components (\code{npc}), returns returns an approximation of the data.
#' The resulting approximation is rescaled and centered to match the original scaling and centering of the input data.
#'
#' @param x Input data as numeric matrix or data frame
#' @param npc Integer specifying the number of principal components retained in approximation.
#' @return Numeric matrix approximating the input data based on the specified number of principal components; scaled and centered to patch input data.
#' @details This function first scales and centers the input data using the \code{scale()} function with \code{center = TRUE} and \code{scale = TRUE}.
#' Then, it performs PCA using the \code{prcomp()} function and extracts the number of principal components specified by \code{npc}.
#' The scaled and centered input data is projected onto these principal components to obtain an initial approximation (\code{x_est}).
#' The function then unscales the approximation (\code{x_est}) using the original scaling and centering parameters (\code{mu} and \code{sigma})
#' obtained from the input data.
#' The resulting approximation (\code{x_est_rescaled}) is a rescaled and centered version of the PCA-based approximation of the input data.
#' When npc is equivalent to the number of columns in \code{x}, then the function output \code{x_est_rescaled} should match \code{x}
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' x = matrix(rnorm(100), nrow = 10)  # Example 10x10 matrix
#'
#' # Perform PCA-based approximation
#' approx_data = pcApprox(x, npc = 3)
#'
#' @export
pcApprox = function(x, npc) {
  # 'pcApprox' performs principal component analysis, then returns an approximation to the data (x) based on a
  # specified number of PCs (npc). The approximation is rescaled and centered to match the original data.

  # First, scale and center the input data
  x_scaled = scale(x, center = TRUE, scale = TRUE)

  # PCA
  PCA_out = prcomp(x_scaled)

  # Extract npc principal components
  PCs = PCA_out$rotation[, 1:npc]

  # Scaled and centered data projected onto PCs
  x_est = as.matrix(x_scaled) %*% PCs
  x_est = x_est %*% t(PCs) # This approximates x_scaled

  # Now, unscale x_est, using descriptives of original x
  mu = attr(x_scaled, "scaled:center")
  sigma = attr(x_scaled, "scaled:scale")
 # Performing element-wise rescaling and removing centering
  x_est_rescaled = sweep(x_est, 2, sigma, "*")
  x_est_rescaled = sweep(x_est_rescaled, 2, mu, "+")

  return(x_est_rescaled)
}

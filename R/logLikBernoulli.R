#' Calculate parameter p that maximizes the log-likelihood of a Bernoulli vector
#'
#' This function takes a given binary vector of 0's or 1's (data) and calculates
#' the parameter p that maximizes the log-likelihood using a grid-based search with
#' p values spaced at 0.001 intervals.
#'
#' @param data A binary vector of 0's and 1's.
#'
#' @return Parameter p that maximizes the log-likelihood for the given data.
#'
#' @details
#' The function computes the log-likelihood for a Bernoulli distribution across a
#' range of p values [0, 1] with step size of 0.001. It returns the p value that
#' maximizes the log-likelihood.
#'
#' @seealso
#' \code{\link{dbern}} for the Bernoulli probability density function.
#'
#' @examples
#' data = c(1, 0, 0, 0, 1, 1, 1)
#' logLikBernoulli(data)
#'
#' @export
logLikBernoulli = function(data) {
  # `logLikBernoulli` takes a given binary vector of 0's or 1's (data) and calculates
  # the parameter `p` that maximizes the log-likelihood using a grid-based search with `p` in steps of 0.001.
  # Computes the log-liklihood across many `p`, then returns the `p` that maximizes the log-likelihood

  # First, confirm that data is a binary vector
  if (length(unique(data)) != 2 || !all(unique(data) %in% c(0, 1))) {
    stop("Restructure input data vector to be a binary vector of 0 and 1")
  }

  # Next, build sub-function that computes the log-likelihood for a Bernoulli distribution
  # for a given p (will run this for many p values in next step)
  log_likelihood_Bern = function(p) {
    sum_data = sum(data)
    n = length(data)
    log_likelihood = sum(data * log(p) + (1 - data) * log(1 - p))
    return(log_likelihood)
  }

  # Now, grid-based search with values of p from [0,1] in steps of 0.001
  p_step = seq(0, 1, by = 0.001)

  # Compute all log-likelihoods
  log_likelihoods = sapply(p_step, log_likelihood_Bern)

  # Find p that maximizes the log-likelihoods
  ind = which.max(log_likelihoods)
  p_max = p_step[ind]

  # Return p_max
  return(p_max)
}

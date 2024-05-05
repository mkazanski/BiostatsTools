#' Calculate minimum sample size for 1-sample or 2-sample t-tests
#'
#' `minimumN` is a wrapper around `pwr::pwr.t.test` that computes the minimum sample size
#' needed for conducting one-sample or two-sample t-tests with pre-specified power and significance
#' levels.
#'
#' @param x1 First sample data as numeric vector for one-sample or two-sample t-tests.
#' @param x2 Second sample data as numeric vector for two-sample t-tests.
#'
#' @return Integer. Minimum sample size required PER GROUP to achieve 80% power at alpha = 0.05
#' for the specified t-test.
#'
#' @details
#' This function performs a power analysis to determine the minimum sample size required
#' for conducting a t-test under specified conditions:
#'
#' - 1-sample t-test: Tests the null hypothesis that the mean of `x1` is 0.
#' - 2-sample t-test: Tests the null hypothesis that the means of `x1` and `x2` are equal.
#'
#' The function uses Cohen's d effect size to estimate the required sample size based on the
#' desired statistical power (80%) and significance level (alpha = 0.05).
#'
#' @examples
#' # Calculate minimum sample size for one-sample t-test
#' minimumN(c(1, 2, 3))
#'
#' # Calculate minimum sample size for two-sample t-test
#' minimumN(c(1, 2, 3), c(4, 5, 6))
#'
#' @import pwr
#' @importFrom effectsize cohens_d
#'
#' @export

minimumN = function(x1,x2 = NULL) {
  # `minimumN` writes a wrapper around pwr::pwr.t.test that takes either one (x1) or two (x2) samples
  # of preliminary data and returns the minimum sample size needed for a t-test of the null hypotheses
  # that either mu(x1) = 0 or mu(x1) = mu(x2) with 80% power at alpha=0.05

  # Necessary packages
  library(pwr)
  library(effectsize)

  # First, determine whether there are 1 or 2 inputs
  test_both = ifelse(is.null(x2), FALSE, TRUE)

  # Power analysis parameters
  power = 0.8
  alpha = 0.05

  # x1 only provided
  if (!test_both) {
    ES = cohens_d(x = x1, mu = 0)
    ES = ES$Cohens_d
    result = pwr.t.test(n = NULL, d = ES, sig.level = alpha, power = power, type = "one.sample", alternative = "two.sided")
    sample_size = ceiling(result$n)
    sprintf("Minimum sample size required for 1-sample t-test: n = %.0f", sample_size)
  } else {
  # x1 and x2 provided
    ES = cohens_d(x1, x2)
    ES = ES$Cohens_d
    result = pwr.t.test(d = ES, power = power, sig.level = alpha, type = "two.sample", alternative = "two.sided")
    sample_size = ceiling(result$n)
    sprintf("Minimum sample size required for 2-sample t-test: n = %.0f in each group", sample_size)
  }

  return(sample_size)
}

#' Standardize variable names to consistent format: small camel case
#'
#' StandardizeNames converts the variable names in the input dataframe or tibble \code{data} to "small camel" case
#' using the janitor package for cleaning and name standardization.
#'
#' @param data A dataframe or tibble with variable names that may require standardization.
#' @return A dataframe identical to \code{data}, but with standardized variable names in small camel case.
#' @import dplyr
#' @import janitor
#' @importFrom dplyr rename_with everything
#' @importFrom janitor make_clean_names
#' @export
#'
#' @examples
#' data = data.frame("First.Column" = 1:5, "Second_Column" = 6:10)
#' data_renames = standardizeNames(data)
#'
standardizeNames = function(data) {
  # `standardizeNames` writes a wrapper around dplyr::rename_with and janitor::make_clean_names
  # that converts the variables in a tibble (data) to "small_camel" case
  # such that variable names are standardized.

  #load libraries
  library(dplyr)
  library(janitor)

  # Ensure input is a dataframe or tibble
  if (!is.data.frame(data) && !is.tibble(data)) {
    stop("Restructure input as a dataframe or tibble.")
  }

  # Replace any punctuation with a space
  cleaned_names = gsub("[[:punct:]]", " ", names(data))

  # Convert variable names to small camel case
  camel_case_names = janitor::make_clean_names(cleaned_names, case = "small_camel")

  # Rename variables in the dataframe using small camel case names
  data_relabeled = rename_with(data, .cols = everything(), .fn = ~ camel_case_names)

  # Return the standardized dataframe
  return(data_relabeled)

}

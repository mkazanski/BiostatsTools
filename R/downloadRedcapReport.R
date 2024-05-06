#' Download Redcap Report
#'
#' Retrieve a specified Redcap report from a given URL using an API token.
#'
#' This function uses \code{Sys.getenv()} to read an API token provided as a string (\code{redcapTokenName}) from the user's .Renviron file. It then retrieves the specified Redcap Report (\code{redcapReportId}) from the given URL (\code{redcapUrl}) and returns the contents as a tibble.
#'
#' @param redcapTokenName Character string specifying the name of the API token in the user's .Renviron file.
#' @param redcapUrl Character string specifying the URL of the Redcap server.
#' @param redcapReportId Numeric specifying the ID of the Redcap report to retrieve.
#'
#' @return A tibble containing the contents of the specified Redcap report.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download Redcap report with specified parameters
#' redcapTokenName = "redcapTokenName"
#' redcapServerUrl = "https://redcap.emory.edu/api/"
#' reportId = 46524
#' myReport = downloadRedcapReport(redcapToken, redcapServerUrl, reportId)
#' }
#'
#' @import tibble
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom httr add_headers
#'
#' @references
#' Redcap API documentation: \url{https://redcap.emory.edu/api/}
#'
#' @seealso
#' \code{\link{httr::POST}}, \code{\link{httr::content}}
#'
#' @family Redcap Functions
#' @name downloadRedcapReport
#' @rdname downloadRedcapReport
#' @aliases downloadRedcapReport
#' @keywords data
#' @concept Redcap
#' @concept httr
#'
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  # `downloadRedcapReport` uses Sys.getenv() to read an API token provided as a string (redcapTokenName) from the users’ .REnviron
  # file (token must be stored as .Renviron), then from the given URL provided in quotations (redcapUrl) retrieves the specified Redcap Report given as a number (redcapReportId).
  # And ultimately returns the contents as a tibble.

  # Required packages
  library("tibble") #for later tibble coercion

  # First, retrieve API token from .REnviron, or stop if not found
  token = Sys.getenv(redcapTokenName)
  if (is.na(token) || token == "") {
    stop("API token not found. Please refer to your .REnviron file to ensure that the token is available.")
  }

  # Next, Prepare API request data using token and redcapReportId
  formData = list(
    "token" = token,
    "content" = "report",
    "format" = "csv",
    "report_id" = as.character(redcapReportId),
    "csvDelimiter" = "",
    "rawOrLabel" = "raw",
    "rawOrLabelHeaders" = "raw",
    "exportCheckboxLabel" = "false",
    "returnFormat" = "csv"
  )

  # Get specified report from redcapUrl
  response = httr::POST(url = redcapUrl, body = formData, encode = "form")

  # Get report as a tibble
  report_tibble = tibble(httr::content(response))

  return(report_tibble)
}

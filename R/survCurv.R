#' Calculate and plot a Kaplan-Meier survival curve
#'
#' \code{survCurv} calculates and plots a Kaplan-Meier survival curve (\eqn{S(t)})
#' based on two independent data arrays of binary event status (1 = event; 0 = censored) and survival time data.
#' Assumption is that each total rows correspond with total participants, so may need to pad inputs if participants do no experience events or are not censored.
#'
#' @param status A numerical vector indicating event status: 1 for event occurred, 0 for censored participant.
#' @param time A numerical vector representing survival times corresponding to event status. Arbitrary units.
#' @return A \code{ggplot} object displaying the Kaplan-Meier survival curve such that axis labeling, legends can be refined
#' @importFrom tibble tibble
#' @importFrom dplyr arrange group_by summarize mutate ungroup
#' @importFrom ggplot2 ggplot geom_step geom_point labs theme_minimal
#' @examples
#' # Load example data (assuming data is available at the provided URL)
#' data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' status = data$status
#' time = data$time
#'
#' # Calculate and plot survival curve
#' survCurv(data$status, data$time)
#'
#' @export
survCurv = function(status, time) {
  # `survCurv` writes a function that takes a numerical vector (status) and a numerical vector (time),
  # and calculates and plots a survival curve S(t).
  # Uses a Kaplan-Meier approach, where each event cuts up the timeline and each participant censored is
  # assumed to not experience an event through at least the next event.

  # Can use sample data from: (https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv).

  # Note that status: 1 = event; 0 = censored

  # Necessary packages
  library("tibble")
  library("dplyr")
  library("ggplot2")

  # First, compile tibble and sort in ascending order temporally
  Data = tibble(status = status, time = time) |>
    arrange(time)

  # How many people enrolled in the study
  n.start = dim(Data)
  n.start = n.start[1] #Assuming all participants either experienced event or were censored, at some point

  # Now, build tibble with event, hazard, survival info
  survData = data |>
    group_by(time) |>
    summarize(n.events = sum(status==1),n.censored = sum(status==0)) |>
    mutate(n.total = n.events + n.censored) |>
    mutate(n.at.risk = lag(n.start-cumsum(n.total),default = n.start)) |>
    mutate(hazard = n.events/n.at.risk) |>
    mutate(survival = cumprod(1-hazard)) |>
    ungroup()

  # Plot the survival curve with censored participants indicated
  s = ggplot(survData, aes(x = time, y = survival)) +
    geom_step() +
    geom_point(data = filter(survData, n.censored>0), aes(x = time, y = survival),
               shape = "+", size = 3, color = "red") +
    labs(x = "time", y = "S(t)", title = "Kaplan-Meier Survival Curve with Censored Events") +
    theme_minimal()

  print(s)

  return(s)
}

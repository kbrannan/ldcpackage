#' Flow Exceedence Calculation
#'
#' Calculats the exceedence time for a vector of flow values
#'
#'
#' @param v.flow, vector: flow data
#' @
#' @return Flow Exceedence values
#' @export
#'

flow.exceed <- function(v.flow) {
  tmp.rank <- rank(v.flow, ties.method = "average")
  tmp.exceed <- tmp.rank / length(v.flow)
  tmp.exceed <- 100 * (1 - tmp.exceed)
  return(tmp.exceed)
}

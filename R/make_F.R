#' A function to make an F list
#' @name make_F
#' @param nyears The number of years in the model
#' @param id The ID of the time series, defaults to 1 assuming there is only one F time series.
#' @param estimated boolean, is F estimated or set?
#' @param delta_method boolean, is the delta method used to generate standard errors?
#' @param phase phase of F estimation
#' @param values a vector of length nyears that assigns the starting or fixed values for F
#' @return the list of F properties and values

make_F <- function(nyears, id = 1, estimated, delta_method, phase, values = NA) {
  F_list <- vector("list")
  F_list$id <- id
  F_list$parameters$estimated <- estimated
  F_list$parameters$delta_method <- delta_method
  F_list$parameters$phase <- phase
  if (!any(is.na(values))) {
    if (length(values) != nyears) {
      stop("The length of the F vector must match the number of years in the model.")
    }
    F_list$parameters$values <- values
  } else {
    F_list$parameters$values <- rep(0.01, nyears)
  }
  return(F_list)
}

#' Creates a likelihood component list for MAS
#' @name make_likelihood_component
#' @param ids A vector of the IDs of each likelihood component in the model
#' @param models A vector with the model distributions for each likelihood type; allowable entries include "lognormal", "multinomial", and "dirichlet"
#' @param lambdas the weighting factors for each likelihood component, one for each year
#' @param parameters only needed for the dirichlet likelihoods, the vectors of observation error parameters
#' @param nyears number of years for each likelihood component
#' @return

make_likelihood_component <- function(ids, models, lambdas, parameters = NA, nyears) {
  likelihood_list <- vector("list")
  for (i in 1:length(ids)) {
    likelihood_list[[i]] <- vector("list")
    likelihood_list[[i]]$id <- ids[i]
    likelihood_list[[i]]$model <- models[i]
    dimensions <- length(lambdas[[i]])
    if (dimensions == nyears) {
      likelihood_list[[i]]$lambda$years <- nyears
      likelihood_list[[i]]$lambda$values <- lambdas[[i]]
    } else {
      break("Error: weighting beyond per-year is not implemented yet.")
    }

    if (length(grep("dirichlet", models[i])) > 0) {
      likelihood_list$parameters <- parameters
    }
  }
  return(likelihood_list)
}

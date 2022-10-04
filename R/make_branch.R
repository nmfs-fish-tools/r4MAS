#' Assign environment variables to a list
#'
#' A function that may be called by `lapply()` to assign all of the variables in the given environment to slots within a list. This
#'
#' @param ... each of the arguments needs to exist in your working directory. If they do, they will be assigned to a list that is then returned.
#' @export
#' @md
make_branch <- function(analyst = "", study_name = "", movement_type = "", years = 1, seasons = 1, ages = 1, extended_plus_group = 1, first_year = 1, last_year = 1, season = 1, recruitment = "", growth = "", selectivity = "", movement = data.frame(1), natural_mortality = data.frame(1), fishing_mortality = data.frame(0.2), area = "", population = "", likelihood_component = "", fleet = "", survey = "") {
  arguments <- ls(sys.frame(sys.parent(0)))[-1]
  function_env <- sys.frame(sys.parent(0))

  return_list <- vector("list")
  for (i in 1:length(arguments)) {
    return_list[[arguments[i]]] <- get(arguments[i], function_env)
    if (mode(return_list[[arguments[[i]]]]) == "list") {
      if (length(return_list[[arguments[i]]]$id) == 0) {
        browser()
      }
    }
  }

  setNames(return_list, arguments)
}

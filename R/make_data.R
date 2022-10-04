#' FUnction to make a data object from input list
#' @name make_data
#' @param config_object a list item generated from reading in a MAS JSON config
#' @param data_object a list item generated from reading in a MAS JSON data file
#' @return a list with populated data objects
make_data <- function(config_object, data_object) {
  config_name <- c("study_name", "analyst")
  arguments <- c(
    "model", "workgroup",
    "years", "seasons", "ages"
  )
  config_name <- c(
    "study_name", "analyst",
    arguments[3:5]
  )

  return_list <- vector("list")

  return_list <- lapply(
    1:length(arguments),
    function(X) return_list[[arguments[X]]] <- config_object[[config_name[X]]]
  )
  setNames(return_list, arguments)

  return_list$data_object <- vector("list")
  for (i in 1:length(data_object[[8]])) {
    return_list$data_object[[i]] <- lapply(1:9, function(x) data_object[[x]][i])
    setNames(return_list$data_object[[i]], c(
      "name",
      "id", "units",
      "data_object_type",
      "sex",
      "areas_of_operation",
      "missing_values",
      "values",
      "observation error"
    ))
  }

  return(return_list)
}

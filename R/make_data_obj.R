#' Function that creates a data object from a list
#' @name make_data_obj
#' @param data_list the data values, a list which can contain multiple data entries
#' @param name a string denoting he name of the data source
#' @param units a string denoting the units of the data source
#' @param data_object_type the type of data (i.e. age composition, index)
#' @param sex the sex the data pertains to
#' @param areas_of_operation the areas this data source is collected in
#' @param observation_error the observation error associated with these data, a list which can contain multiple entries, each one is assumed to match an entry of data_list
#' @return a list containing the data arguments
make_data_obj <- function(data_list, name, units, data_object_type, sex, areas_of_operation, observation_error) {
  arguments <- ls(sys.frame(sys.parent(0)))[-1]
  function_env <- sys.frame(sys.parent(0))
  ret <- lapply(arguments[2:6], get, function_env)
  ret[[6]] <- seq(1, length(ret[1]))
  ret[[7]] <- rep(-999, length(ret[[1]]))
  ret[[8]] <- data_list
  ret[[9]] <- observation_error

  return(ret)
}

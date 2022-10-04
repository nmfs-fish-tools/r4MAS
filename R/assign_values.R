#' Assign environment variables to a list
#' @name assign_values
#' @description A function that may be called by `lapply()` to assign all of the variables in the given environment to slots within a list.
#'
#' @param X - the name of the variable in `function_env` to assign to the list slot named `X`
#' @param function_env - the environment in which you want the function to search for the variable named `X`
#' @param return_list - the list item you want to add the value of `X` to in the slot named `X`
#' @seealso [base::get()]
#' @export
#' @md
assign_values <- function(X, function_env, return_list) {
  return_list$X <- get(X, function_env)
  return(return_list)
}

#' Make a list of everything in function environment
#' @name make_list
#' @description A function that returns a list representing the parent environment
#' @return a list with the associated names and values of the environment objects
#' @export
#' @md
make_list <- function() {
  function_env <- sys.frame(sys.parent(0))
  arguments <- ls(function_env)[-1]
  return_list <- vector("list")
  return_list <- lapply(X = arguments, function(X) return_list$X <- get(X, function_env))
  setNames(return_list, arguments)
}

#' Assign environment variables to a list
#'
#' A function that creates a list with each element in the environment as a list slot
#'
#' @param list__ - the list you want to affix objects to slots in
#' @param element_type - a vector with the names of the slots you want the value to be assigned to in the list
#' @param element_value - a vector with the values of the slots you are adding to the list
#' @return the list with added slots
#' @seealso [magrittr::inset2()]
#' @export
#' @md
generic_assignment <- function(list__, element_type, element_value) {
  list__[[get(element_type, parent.frame)]] <- element_value
  return(list__)
}

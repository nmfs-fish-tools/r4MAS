#' Creates a movement matrix for MAS input
#' @name movement_matrix
#' @param recruits transition probability of recruits from one area to another
#' @param males transition probability of males from one area to another
#' @param females transition probability of females from one area to another
#' @return the list with populated movement information
movement_matrix <- function(recruits, males, females) {
  move_list <- vector("list")
  move_list$`id` <- 1
  move_list$estimated <- "false"
  move_list$recruits <- vector("list")
  move_list$recruits[[1]] <- recruits
  move_list$males <- vector("list")
  move_list$males[[1]] <- males
  move_list$females <- vector("list")
  move_list$females[[1]] <- females
  return(move_list)
}

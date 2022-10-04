#' Create season attribute of the model
#'
#' A function returns a list with season attributes
#'
#' @param season_names - a vector of season names
#' @param number_of_seasons - the number of seasons
#' @param months_per_season - a vector, where each object is the number of months in each season
#' @return season_list a list object containing the seasons, where each season is a nested list with attributes id, name, and months for each season
#' @export
#' @md
create_seasons <- function(season_names, number_of_seasons, months_per_season) {
  season_tmp <- season_list <- vector("list")
  for (i in 1:number_of_seasons) {
    season_tmp$id <- i
    season_tmp$name <- season_names[i]
    season_tmp$months <- months_per_season[i]
    season_list$season <- season_tmp
  }
  return(season_list)
}

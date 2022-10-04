#' This function writes a JSON configuration file for MAS.
#'
#' @param config_list The list of objects, where each slot corresponds to a MAS config input
#' @param outfile_path The path to the config file you want to save. This should end in a .json file extension.
#' @return a string comprising the JSON config file

write_config <- function(config_list, outfile_path) {
  # Write out config .json file
  jsonlite::write_json(config_list, outfile_path,
    pretty = TRUE,
    auto_unbox = TRUE, digits = 6, dup_names = TRUE
  )
}

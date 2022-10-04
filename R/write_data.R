#' This function writes a JSON data input file for MAS
#' @name write_data
#' @param data_list the list of data inputs, where each list slot corresponds to a data type needed by MAS
#' @param outfile_path the path to the data file you want to write. This should end with a .json file extension.
#' @return the string containing the json data file to write.

write_data <- function(data_list, outfile_path) {
  # Write out data .json file
  jsonlite::write_json(data_list, outfile_path,
    pretty = TRUE,
    auto_unbox = TRUE, digits = 6, dup_names = TRUE
  )
}

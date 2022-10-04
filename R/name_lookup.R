#' Function to map stock assessment quantities between platforms
#' @name name_lookup
#' @author Christine Stawitz
#' @description Function to map names between different stock assessment models
#' @param input_format string; the model the data or output is coming from
#' @param names string or vector of strings; names to map
#' @param lookup data frame that contains the lookup table to map in between different models
#' @return new_name - the mapped name
#'
#' @example name_lookup(input_format = "SS", names = "SR_LN(R0)")
name_lookup <- function(input_format, names, lookup) {

  # Identify strings to look for
  search_strings <- lookup %>%
    filter(type %in% c("parameter", "derived_quantity")) %>%
    select(input_format)

  # Get row index corresponding to sex names
  female <- which(lookup$name == "female")
  male <- which(lookup$name == "male")

  # Remove blanks
  search_strings <- search_strings[!search_strings == ""]

  # Define objects needed
  ind <- rep("", length(names))
  new_name <- data.frame(matrix(NA, nrow = length(names), ncol = 4))
  names(new_name) <- c("name", "sex", "fleet", "year")

  get_name <- function(lookup_name) {
    return(lookup[lookup_name, "output"])
  }

  # Define cases
  # case 1: 1 parameter for whole population
  # Case 2: 1 parameter for females only
  # Case 3: 2 parameters total: 1 parameter each for males and females
  # Case 4: multiple parameters: year-specific parameters
  # Case 5: multiple parameters: 1 parameter per fleet

  for (i in 1:length(search_strings)) {

    # Find which lookup table entry corresponds to the current string
    matches <- grep(search_strings[i], names, fixed = TRUE)
    lookup_index <- grep(search_strings[i], lookup[, input_format], fixed = TRUE)

    # Indices for female- and male- specific parameter value
    fem_par <- grep(lookup[female, input_format], names[matches])
    male_par <- grep(lookup[male, input_format], names[matches])

    # Look for strings corresponding to years in the parameter names
    year_regexpr <- "_\\d{4}$"
    year_par <- grep(year_regexpr, names[matches])

    # Case 1: One parameter matches; ex: steepness
    case1_bool <- length(matches) == 1

    # Case 2: One female parameter matches; ex: fecundity
    case2_bool <- (length(matches) == 1) && (length(fem_par) == 1) && (length(male_par) == 0)

    # Case 3: Sex-specific parameters; ex: sex-specific M
    case3_bool <- (length(fem_par) == 1) && (length(male_par) == 1)

    # Case 4: Parameters with years in them; ex: time-varying selectivity
    case4_bool <- (length(year_par) > 0)


    if (case1_bool) {
      # If only one parameter matches, put in 'name' column
      new_name[matches, 1] <- get_name(lookup_index)
      new_name[matches, 2] <- get_name(female)

      # Increment counter
      # new_names_rows <- new_names_rows + 1
    } else if (case3_bool) {

      # In this case, there is one parameter for each sex - denoted by containing corresponding strings for "female" and "male" in it
      sex_index <- c(matches[fem_par], matches[male_par])

      # Double-check sex index and matches correspond
      assertthat::assert_that((sex_index == matches) || sex_index == rev(matches), msg = "Sex index doesn't match parameter names")

      # If so, there is one version of this parameter for each sex
      new_name[sex_index, 1] <- get_name(lookup_index)
      new_name[sex_index, 2] <- get_name(c(female, male))
      # new_name_rows <- new_names_rows + 2
    } else if (case2_bool) {
      # If so, there is one version of this parameter total but it is female-specific
      new_name[matches[fem_par], 1] <- get_name(lookup_index)
      new_name[matches[fem_par], 2] <- get_name(female)
      # new_name_rows <- new_names_rows + 1
    } else if (case4_bool) {
      # We know there are values corresponding to year, but may be >1 corresponding to yr
      without_year <- sub(year_regexpr, "", names[matches])
      if (all(without_year == without_year[1])) {
        # If year is the only unique part of variable name, these are values per year
        year_pos <- regexec(year_regexpr, names[matches])
        # Extract years with specific parameter values
        years <- regmatches(names[matches], year_pos)

        # Give each parameter the same name so it can be a vector
        new_name[matches, 1] <- rep(get_name(lookup_index), length(years))
        new_name[matches, 4] <- years

        # Increment counter
        # new_name_rows <- new_name_rows + length(years)
      } else {
        # If year is not the only unique part of the name, there might be sex- or fleet-specific parameters
      }
    }
  }
  return(new_name)
}

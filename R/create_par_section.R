#' A function to create a section of the parameter input for each MAS type
#' @name create_par_section
#' @param section_type The type of section; string of type for example "recruitment", "growth".
#' @param section_type_object The object created within the RMAS interface denoting the type of function used
#' @param id The id of this parameter section; if there's only one parameter section of this type in the model, leave blank to assign the default ID of 1.
#' @param par_names vector of the parameter names, must match the names denoted in the RMAS C++ file
#' @param par_lo vector of the parameter lower bounds, default NULL will assign default lower bound
#' @param par_hi vector of the parameter upper bounds, default NULL will assign default upper bound
#' @param par_units string denoting the units of the parameter, default NULL will assume the parameter is unitless
#' @param par_phase vector of estimation phases for this parameter, default NULL will assume this uses the MAS default
#' @param par_value vector of values for this parameter, note this will be translated to C++ and cast into the appropriate type (for example, double, integer)
#' @param  rec_devs supply only if this parameter section is of type "recruitment"
#' @return the \code{section_type_object} with the populated parameter values
create_par_section <- function(section_type, section_type_object, id = 1,
                               par_names, par_lo = NULL,
                               par_hi = NULL, par_units = NULL, par_phase = NULL,
                               par_value, rec_devs = NA) {
  check_if_null <- function(x, ind) {
    bool <- FALSE
    if (is.null(x)) {
      bool <- TRUE
    } else if (is.na(x[ind])) {
      bool <- TRUE
    }
    return(bool)
  }

  for (x in 1:length(par_names)) {
    if (par_names[x] == "recdevs") {
      section_type_object$estimate_deviations <- rec_devs[[1]]
      section_type_object$constrained_deviations <- rec_devs[[2]]
      if (rec_devs[[2]]) {
        section_type_object$deviations_min <- par_lo[x]
        section_type_object$deviations_max <- par_hi[x]
      }
      section_type_object$deviation_phase <- par_phase[x]
      recruitment$SetDeviations(rec_devs[[3]])
    } else {
      section_type_object[[par_names[x]]]$value <- par_value[x]
      if (!check_if_null(par_units, x)) {
        section_type_object[[par_names[x]]]$units <- par_units[x]
      }

      if (!check_if_null(par_phase, x)) {
        if (par_phase[x] < 0) {
          section_type_object[[par_names[x]]]$estimated <- FALSE
        } else {
          section_type_object[[par_names[x]]]$estimated <- TRUE
          section_type_object[[par_names[x]]]$phase <- par_phase[x]
        }
      }
      if (!check_if_null(par_lo, x)) {
        section_type_object[[par_names[x]]]$min <- par_lo[x]
      }
      if (!check_if_null(par_hi, x)) {
        section_type_object[[par_names[x]]]$max <- par_hi[x]
      }
    }
  }


  return(section_type_object)
}

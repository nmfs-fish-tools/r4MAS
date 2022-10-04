
plot_output_object <- function(json_output) {
  years <- json_output$nyears
  ages <- json_output$nages
  popdy <- json_output$population_dynamics
  observed_catch_data <- popdy$fleet$observed
  expected_catch_data <- popdy$fleet$undifferentiated
  expected_surv_data <- popdy$survey$undifferentiated
  observed_surv_data <- popdy$survey$observed

  names(expected_catch_data)[c(2, 4)] <- names(observed_catch_data)

  ind_surv <- names(expected_surv_data[which(names(expected_surv_data) %in% names(observed_surv_data))])

  ind_catch <- names(expected_catch_data[which(names(expected_catch_data) %in% names(observed_catch_data))])

  make_mat <- function(data_list) {
    if (length(unlist(data_list)) == years * ages) {
      return(matrix(unlist(data_list), nrow = ages, ncol = years, byrow = FALSE))
    } else {
      return(unlist(data_list))
    }
  }

  e_catch <- lapply((1:length(ind_catch)), function(x) make_mat(expected_catch_data[[ind_catch[x]]]$values))
  o_catch <- lapply((1:length(ind_catch)), function(x) make_mat(observed_catch_data[[ind_catch[x]]]$values))

  e_surv <- lapply(
    (1:length(ind_surv)),
    function(x) make_mat(expected_surv_data[[ind_surv[x]]]$values)
  )
  o_surv <- lapply(
    (1:length(ind_surv)),
    function(x) make_mat(observed_surv_data[[ind_surv[x]]]$values)
  )


  plot_obs_vs_exp(o_surv, e_surv)
  return(invisible(1))
}

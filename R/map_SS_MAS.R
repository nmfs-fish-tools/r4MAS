#' Function mapping SS values to MAS
#' @name map_SS_MAS
#' @author Christine Stawitz
#' @description Function to map names between different stock assessment models
#' @param toMAS boolean; if this will be written to a MAS-style JSON input
#' @param in_name string; the string to map to a new name
#' @return new_name - the mapped name
#'
#' @example map_SS_MAS("SR_LN(R0)")
map_SS_MAS <- function(toMAS = TRUE, in_name) {
  if (toMAS) {
    new_name <- switch(in_name,
      "L_at_Amin_Fem_GP_1" = "lmin",
      "L_at_Amax_Fem_GP_1" = "lmax",
      "L_at_Amin__GP_1" = "lmin",
      "Wtlen_1_Fem" = "alpha_f",
      "Wtlen_2_Fem" = "beta_f",
      "NatM_p_1_Fem_GP_1" = "M",
      "SR_LN(R0)" = "R0",
      "SR_BH_steep" = "h",
      "SR_sigmaR" = "sigma_r",
      "SR_autocorr" = "rho",
      "VonBert_K_Fem_GP_1" = "c",
      "AgeSel_P1_Fishery(1)" = "a50",
      "AgeSel_P2_Fishery(1)" = "s",
      "AgeSel_P1_Acoustic_Survey(2)" = "a50",
      "AgeSel_P2_Acoustic_Survey(2)" = "s",
      in_name
    )
  } else {
    new_name <- switch(in_name,
      "L_at_Amin_Fem_GP_1" = "lmin",
      "L_at_Amax_Fem_GP_1" = "lmax",
      "L_at_Amin__GP_1" = "lmin",
      "Wtlen_1_Fem" = "alpha_f",
      "Wtlen_2_Fem" = "beta_f",
      "NatM_p_1_Fem_GP_1" = "M",
      "SR_LN(R0)" = "R0",
      "SR_BH_steep" = "h",
      "SR_sigmaR" = "sigma_r",
      "SR_autocorr" = "rho",
      "VonBert_K_Fem_GP_1" = "c",
      in_name
    )
  }
  return(new_name)
}

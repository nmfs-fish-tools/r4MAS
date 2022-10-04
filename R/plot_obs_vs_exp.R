plot_obs_vs_exp <- function(obs_list, exp_list, ages = 1:12, years = 1:30) {
  ind <- length(obs_list)
  for (i in 1:length(ind)) {
    obs <- obs_list[[i]]
    exp <- exp_list[[i]]

    if (is.null(dim(obs))) {
      exp <- apply(exp, 2, sum)
      pdf(paste0(
        "ObsvsExpected", pattern_obs[i],
        ".pdf"
      ))
      plot(obs ~ years, pch = 19, main = paste(pattern_obs[i]))
      lines(exp ~ years)
      dev.off()
    } else {
      if (any(dim(obs) != dim(exp))) {
        if (dim(obs)[2] == dim(exp)[2]) {
          pdf(paste0("ObsVsExpected", pattern_obs[i], ".pdf"))
          par(mfrow = c(1, 1))
          exp <- apply(exp, 2, sum)
          plot(as.numeric(obs) ~ years, pch = 19, main = pattern_obs[i])
          lines(exp ~ years)
          dev.off()
        } else {
          print("dimensions don't match")
        }
      } else {
        if (dim(obs)[1] > 1) {
          # pdf(paste0("ObsVsExpected", i,
          #       ".pdf"))
          par(mfrow = c(2, 2))
          for (j in 1:ncol(obs)) {
            plot(obs[, j] ~ ages, pch = 19, main = paste(i, j))
            lines(exp[, j] ~ ages)
          }

          # dev.off()
        } else {
          pdf(paste0(
            "ObsVsExpected", sub(": ", "", pattern_obs[i]),
            ".pdf"
          ))
          par(mfrow = c(1, 1))
          plot(as.numeric(obs) ~ years, pch = 19, main = pattern_obs[i])
          lines(as.numeric(exp) ~ years)
          dev.off()
        }
      }
    }
  }
}

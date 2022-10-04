#' Function to read in a MAS output txt file (deprecated) and create plots.
#' @name output_plots
#' @param data.dir the directory where the output txt is stored
#' @param ages a vector of the ages in the model
#' @param years a vector of the years in the model
#' @param pop_name a string denoting the name of the population file
#' @param rep_name a string denoting the name of the report file
#' @param figs_dir the directory where output figures should be stored
#' @return a list with two entries - parameter values and N at age

output_plots <- function(data.dir, ages, years, pop_name, rep_name, figs_dir) {
  setwd(here(data.dir))

  # Read in populations.txt and report file
  MAS_pops <- scan(pop_name,
    what = "character",
    flush = TRUE,
    blank.lines.skip = FALSE,
    quiet = TRUE,
    sep = "\n"
  )
  idx <- which(MAS_pops != "")
  non_blanks <- MAS_pops[idx]

  # Get scalar parameters
  rec_params <- c("R0\t", "h\t", "SB0\t")
  id_rec <- unlist(lapply(rec_params, grep, non_blanks))
  rec_param_list <- strsplit(non_blanks[id_rec], "\t")
  # Get vector and matrices for each sex
  get_each_sex <- function(sex, non_blanks, id_sex, name_diff) {

    # Identify strings corresponding to sex
    id_sex <- unlist(lapply(sex, grep, non_blanks))
    id_sex_total <- unlist(lapply("Total", grep, non_blanks[id_sex + 1]))
    # Name of the quantity is the string before sex
    # if sex is MF, otherwise it is "Males and Females"
    # or "Total"
    if (sex %in% c("Male", "Female")) {
      names_sex <- paste(
        non_blanks[id_sex - name_diff],
        sex
      )
    } else {
      id_sex <- id_sex[id_sex_total]
      names_sex <- paste(
        non_blanks[id_sex - name_diff],
        "Total"
      )
    }
    vals_sex <- vector("list")

    # Get values for each quantity
    get_vals <- function(i, k) {
      return(as.double(unlist(strsplit(non_blanks[id_sex[i] + k], " "))))
    }

    for (i in 1:length(id_sex)) {
      # Find first non-string value following the name
      for (j in 1:5) {
        if (!any(is.na(get_vals(i, j)))) {
          k <- j
          break
        }
      }
      tmp <- NULL
      while (!any(is.na(get_vals(i, k)))) {
        tmp <- rbind(tmp, get_vals(i, k))
        k <- k + 1
      }
      vals_sex[[names_sex[i]]] <- tmp
    }
    return(vals_sex)
  }

  # List of populations values
  fem_list <- get_each_sex("Female", non_blanks, idx, 1)
  male_list <- get_each_sex("Male", non_blanks, idx, 1)


  if (!dir.exists(here(figs_dir))) {
    dir.create(here(figs_dir))
  }

  each_plot <- function(input_list) {
    # Names of each quantity
    if (length(grep("plots", getwd())) == 0) {
      setwd(here(figs_dir))
    }
    quants <- names(input_list)

    for (i in 1:length(input_list)) {
      curr <- input_list[[quants[i]]]
      if (nrow(curr) <= 1) {
        png(paste0(quants[i], ".png"))
        dim_vect <- length(curr)
        if (dim_vect == length(ages)) {
          plot(curr[1, ] ~ ages,
            main = quants[i],
            xlab = "Ages", type = "l",
            ylab = quants[i]
          )
        } else if (dim_vect == length(years)) {
          plot(curr[1, ] ~ years,
            main = quants[i],
            xlab = "Yrs",
            ylab = quants[i],
            type = "l"
          )
        } else {
          print("dimensions do not match!")
        }
        dev.off()
      } else {
        if (nrow(curr) == length(ages)) {
          by_age <- apply(curr, 1, sum)
          by_yr <- apply(curr, 2, sum)
          png(paste0(quants[i], "by_age.png"))
          plot(by_age ~ ages,
            type = "l",
            main = quants[i]
          )
          dev.off()

          png(paste0(quants[i], "by_yr.png"))
          plot(by_yr ~ years,
            type = "l",
            main = quants[i]
          )
          dev.off()

          pdf(paste0(quants[i], ".pdf"))
          par(mfrow = c(2, 2))
          for (j in 1:length(years)) {
            plot(curr[, j] ~ ages,
              type = "l",
              main = paste(quants[i], years[j])
            )
          }
          dev.off()
        } else if (nrow(curr) == length(years)) {
          pdf(paste0(quants[i], ".pdf"))
          par(mfrow = c(2, 2))
          for (j in 1:length(years)) {
            plot(curr[j, ] ~ ages,
              type = "l",
              main = paste(quants[i], years[j])
            )
          }
          dev.off()
        } else {
          print("Dimensions do not match!")
        }
      }
    }
    setwd(here())
  }
  each_plot(fem_list)
  each_plot(male_list)



  setwd(here(data.dir))
  MAS_rep <- scan(rep_name,
    what = "character",
    flush = TRUE,
    blank.lines.skip = FALSE,
    quiet = TRUE,
    sep = "\n"
  )

  idx <- which(MAS_rep != "")
  non_blanks <- MAS_rep[idx]

  exp_report <- get_each_sex("Expected", non_blanks, id_sex = idx, 0)
  obs_report <- get_each_sex("Observed", non_blanks, id_sex = idx, 0)

  pattern_obs <- sub("Observed ", "", names(obs_report))
  pattern_obs <- sub(": O", "", pattern_obs)
  ind <- unlist(lapply(pattern_obs,
    grep,
    x = names(exp_report)
  ))

  for (i in 1:length(ind)) {
    obs <- obs_report[[names(obs_report)[i]]]
    exp <- exp_report[[names(exp_report)[ind[i]]]]
    if (length(grep("plots", getwd())) == 0) {
      setwd(here(figs_dir))
    }
    if (is.null(dim(obs))) {
      exp <- apply(exp, 2, sum)
      png(paste0(
        "ObsvsExpected", pattern_obs[i],
        ".png"
      ))
      plot(obs ~ years, pch = 19, main = paste(pattern_obs[i]))
      lines(exp ~ years)
      dev.off()
    } else {
      if (any(dim(obs) != dim(exp))) {
        if (dim(obs)[2] == dim(exp)[2]) {
          png(paste0("ObsVsExpected", pattern_obs[i], ".png"))
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
          pdf(paste0(
            "ObsVsExpected",
            strsplit(
              pattern_obs[i],
              ":"
            )[[1]][1],
            ".pdf"
          ))
          par(mfrow = c(2, 2))
          for (j in 1:ncol(obs)) {
            plot(obs[, j] ~ ages, pch = 19, main = paste(pattern_obs[i], j))
            lines(exp[, j] ~ ages)
          }

          dev.off()
        } else {
          png(paste0(
            "ObsVsExpected", sub(": ", "", pattern_obs[i]),
            ".png"
          ))
          par(mfrow = c(1, 1))
          plot(as.numeric(obs) ~ years, pch = 19, main = pattern_obs[i])
          lines(as.numeric(exp) ~ years)
          dev.off()
        }
      }
    }
  }

  est_header <- grep("Estimated", non_blanks)
  est_area <- grep("Area", non_blanks)
  par_entries <- non_blanks[(est_header + 1):(est_area - 1)]
  par_split <- unlist(strsplit(par_entries, " "))
  par_nonblanks <- par_split[which(par_split != "")]
  par_mat <- matrix(par_nonblanks[4:length(par_nonblanks)], ncol = 3, byrow = T)

  par_mat <- data.frame(par_mat)
  names(par_mat) <- par_nonblanks[1:3]
  # Return only non-empty MAS report file entries
  n_header <- grep("Numbers", non_blanks)
  n_at_age <- non_blanks[(n_header[1] + 1):(n_header[1] + length(ages))]

  return(list("parameters" = par_mat, "n_at_age" = n_at_age))
}

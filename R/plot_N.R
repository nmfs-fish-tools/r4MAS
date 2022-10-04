#' A function to plot N at age
#' @name plot_NAtAge
#' @param SS the matrix of numbers at age from Stock Synthesis, where rows are ages
#' @param MAS the matrix of numbers at age from MAS, where rows are ages
#' @param pdf_on boolean, should the PDF be created? Default is \code{TRUE}
#' @param byage boolean, should the numbers be plotted with each plot representing an age (default to FALSE) or a year?
#' @param years the vector of years in the model
#' @param ages the vector of ages in the model
plot_NAtAge <- function(SS, MAS, pdf_on = TRUE, byage = FALSE, years, ages) {
  if (byage) {
    if (pdf_on) {
      pdf("plots/NAtAge_ByAge.pdf")
    }
    for (i in 1:nrow(SS)) {
      plot(NA,
        xlim = c(min(years), max(years)),
        ylim = c(
          min(rbind(MAS[i, ], SS[i, ])),
          max(rbind(SS[i, ], MAS[i, ]))
        ),
        xlab = "Year", ylab = "Number",
        main = paste("Age", ages[i])
      )
      points(as.numeric(SS[i, ]) ~ years)
      lines(as.numeric(MAS[i, ]) ~ years)
    }

    if (pdf_on) {
      dev.off()
    }
  } else {
    if (pdf_on) {
      pdf("plots/NAtAge_ByYr.pdf")
    }

    for (i in 1:ncol(SS)) {
      plot(NA,
        xlim = c(min(ages), max(ages)),
        ylim = c(
          min(cbind(MAS[, i], SS[, i])),
          max(cbind(SS[, i], MAS[, i]))
        ),
        main = paste("Year", years[i]), xlab = "Age",
        ylab = "Number"
      )
      points(as.numeric(SS[, i]) ~ ages)
      lines(as.numeric(MAS[, i]) ~ ages)
    }

    if (pdf_on) {
      dev.off()
    }
  }
}

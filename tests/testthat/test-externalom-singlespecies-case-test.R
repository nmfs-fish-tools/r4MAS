
# Test RMAS: single species without recruitment bias correction case test  -------------------------------------------------------------------
# Initialize RMAS module, input data, and model settings
# Run MAS model
# Compare MAS model outputs with true values from model comparison OM

test_that(
  "External OM test: singlespecies without bias correction in recruitment",
  {

    # Load module
    r4mas <- Rcpp::Module("rmas", dyn.load(dll_path))
    test_path <- file.path(test_example_path, "externalom_example")
    load(file.path(test_path, "singlespecies.RData"))

    # General settings
    nyears <- om_input$nyr
    nseasons <- 1
    nages <- om_input$nages
    ages <- om_input$ages

    # Define area
    area1 <- new(r4mas$Area)
    area1$name <- "area1"

    # Recruitment
    recruitment <- new(r4mas$BevertonHoltRecruitment)
    recruitment$R0$value <- om_input$R0 / 1000
    recruitment$R0$estimated <- TRUE
    recruitment$R0$phase <- 1
    recruitment$h$value <- om_input$h
    recruitment$h$estimated <- FALSE
    recruitment$h$phase <- 3
    recruitment$h$min <- 0.2001
    recruitment$h$max <- 1.0
    recruitment$sigma_r$value <- om_input$logR_sd
    recruitment$sigma_r$estimated <- TRUE
    recruitment$sigma_r$min <- 0
    recruitment$sigma_r$max <- 1.0
    recruitment$sigma_r$phase <- 2
    recruitment$estimate_deviations <- TRUE
    recruitment$constrained_deviations <- TRUE
    recruitment$deviations_min <- -15.0
    recruitment$deviations_max <- 15.0
    recruitment$deviation_phase <- 1
    recruitment$SetDeviations(om_input$logR.resid)
    recruitment$use_bias_correction <- FALSE

    # Growth
    growth <- new(r4mas$VonBertalanffyModified)
    empirical_weight <- rep(om_input$W.kg, times = om_input$nyr)
    survey_empirical_weight <- replicate(nages * nyears, 1.0)
    growth$SetUndifferentiatedCatchWeight(empirical_weight)
    growth$SetUndifferentiatedSurveyWeight(survey_empirical_weight)
    growth$SetUndifferentiatedWeightAtSeasonStart(empirical_weight)
    growth$SetUndifferentiatedWeightAtSpawning(empirical_weight)
    growth$a_min$value <- 0.01
    growth$a_max$value <- 10.0
    growth$c$value <- 0.3
    growth$lmin$value <- 5
    growth$lmax$value <- 50
    growth$alpha_f$value <- 0.000025
    growth$alpha_m$value <- 0.000025
    growth$beta_f$value <- 2.9624
    growth$beta_m$value <- 2.9624

    # Maturity
    maturity <- new(r4mas$Maturity)
    maturity$values <- om_input$mat.age

    # Natural Mortality
    natural_mortality <- new(r4mas$NaturalMortality)
    natural_mortality$SetValues(om_input$M.age)

    # Define Movement (only 1 area in this model)
    movement <- new(r4mas$Movement)
    movement$connectivity_females <- c(0.0)
    movement$connectivity_males <- c(0.0)
    movement$connectivity_recruits <- c(0.0)

    # Initial Deviations
    initial_deviations <- new(r4mas$InitialDeviations)
    initial_deviations$values <- rep(0.0, times = om_input$nages)
    initial_deviations$estimate <- TRUE
    initial_deviations$phase <- 2

    population <- new(r4mas$Population)
    for (y in 1:(nyears))
    {
      population$AddMovement(movement$id, y)
    }

    population$AddNaturalMortality(natural_mortality$id, area1$id, "undifferentiated")
    population$AddMaturity(maturity$id, area1$id, "undifferentiated")
    population$AddRecruitment(recruitment$id, 1, area1$id)
    population$SetInitialDeviations(initial_deviations$id, area1$id, "undifferentiated")
    population$SetGrowth(growth$id)
    population$sex_ratio <- 0.5

    # Fishing Mortality
    fishing_mortality <- new(r4mas$FishingMortality)
    fishing_mortality$estimate <- TRUE
    fishing_mortality$phase <- 1
    fishing_mortality$min <- 0.0
    fishing_mortality$max <- 4
    fishing_mortality$SetValues(om_output$f)

    # Selectivity Model
    fleet_selectivity <- new(r4mas$LogisticSelectivity)
    fleet_selectivity$a50$value <- om_input$sel_fleet$fleet1$A50.sel
    fleet_selectivity$a50$estimated <- TRUE
    fleet_selectivity$a50$phase <- 2
    fleet_selectivity$a50$min <- 0.0
    fleet_selectivity$a50$max <- max(om_input$ages)

    fleet_selectivity$slope$value <- 1 / om_input$sel_fleet$fleet1$slope.sel
    fleet_selectivity$slope$estimated <- TRUE
    fleet_selectivity$slope$phase <- 2
    fleet_selectivity$slope$min <- 0
    fleet_selectivity$slope$max <- max(om_input$ages)

    survey_selectivity <- new(r4mas$LogisticSelectivity)
    survey_selectivity$a50$value <- om_input$sel_survey$survey1$A50.sel
    survey_selectivity$a50$estimated <- TRUE
    survey_selectivity$a50$phase <- 2
    survey_selectivity$a50$min <- 0.0
    survey_selectivity$a50$max <- max(om_input$ages)

    survey_selectivity$slope$value <- 1 / om_input$sel_survey$survey1$slope.sel
    survey_selectivity$slope$estimated <- TRUE
    survey_selectivity$slope$phase <- 2
    survey_selectivity$slope$min <- 0
    survey_selectivity$slope$max <- max(om_input$ages)

    # Index data
    catch_index <- new(r4mas$IndexData)
    catch_index$values <- em_input$L.obs$fleet1
    catch_index$error <- rep(em_input$cv.L$fleet1, times = om_input$nyr)

    survey_index <- new(r4mas$IndexData)
    survey_index$values <- em_input$survey.obs$survey1
    survey_index$error <- rep(em_input$cv.survey$survey1, times = om_input$nyr)


    # Age Comp Data
    catch_comp <- new(r4mas$AgeCompData)
    catch_comp$values <- as.vector(t(em_input$L.age.obs$fleet1))
    catch_comp$sample_size <- rep(em_input$n.L$fleet1, nyears * nseasons)

    survey_comp <- new(r4mas$AgeCompData)
    survey_comp$values <- as.vector(t(em_input$survey.age.obs$survey1))
    survey_comp$sample_size <- rep(em_input$n.survey$survey1, times = om_input$nyr)

    # NLL models
    fleet_index_comp_nll <- new(r4mas$Lognormal)
    fleet_index_comp_nll$use_bias_correction <- FALSE
    fleet_age_comp_nll <- new(r4mas$Multinomial)

    survey_index_comp_nll <- new(r4mas$Lognormal)
    survey_index_comp_nll$use_bias_correction <- FALSE
    survey_age_comp_nll <- new(r4mas$Multinomial)

    # Fleet
    fleet <- new(r4mas$Fleet)
    fleet$AddAgeCompData(catch_comp$id, "undifferentiated")
    fleet$AddIndexData(catch_index$id, "undifferentiated")
    fleet$SetAgeCompNllComponent(fleet_age_comp_nll$id)
    fleet$SetIndexNllComponent(fleet_index_comp_nll$id)
    fleet$AddSelectivity(fleet_selectivity$id, 1, area1$id)
    fleet$AddFishingMortality(fishing_mortality$id, 1, area1$id)


    # Survey
    survey <- new(r4mas$Survey)
    survey$AddAgeCompData(survey_comp$id, "undifferentiated")
    survey$AddIndexData(survey_index$id, "undifferentiated")
    survey$SetIndexNllComponent(survey_index_comp_nll$id)
    survey$SetAgeCompNllComponent(survey_age_comp_nll$id)
    survey$AddSelectivity(survey_selectivity$id, 1, area1$id)
    survey$q$value <- em_input$survey_q$survey1
    survey$q$min <- 0
    survey$q$max <- 10
    survey$q$estimated <- TRUE
    survey$q$phase <- 1

    # Build the MAS model
    mas_model <- new(r4mas$MASModel)
    mas_model$compute_variance_for_derived_quantities <- FALSE
    mas_model$nyears <- nyears
    mas_model$nseasons <- nseasons
    mas_model$nages <- nages
    mas_model$extended_plus_group <- max(om_input$ages)
    mas_model$ages <- ages
    mas_model$AddFleet(fleet$id)
    mas_model$catch_season_offset <- 0.0
    mas_model$spawning_season_offset <- 0.0
    mas_model$survey_season_offset <- 0.0

    mas_model$AddSurvey(survey$id)
    mas_model$AddPopulation(population$id)
    mas_model$Run()

    write(mas_model$GetOutput(),
      file = file.path(test_path, "test_output.json")
    )

    mas_model$Reset()

    # Read output
    args <- commandArgs(trailingOnly = TRUE)
    args <- "test_output.json"
    json_output <- read_json(file.path(test_path, args[1]))
    popdy <- json_output$population_dynamics
    pop <- popdy$populations[[1]]
    flt <- popdy$fleets[[1]]
    srvy <- popdy$surveys[[1]]

    parameter <- unlist(json_output$estimated_parameters$parameters)
    parameter_table <- as.data.frame(matrix(parameter, ncol = 3, byrow = TRUE))
    colnames(parameter_table) <- c(
      "Parameter",
      "Value",
      "Gradient"
    )
    parameter_table$Value <- round(as.numeric(parameter_table$Value),
      digits = 6
    )
    parameter_table$Gradient <- round(as.numeric(parameter_table$Gradient),
      digits = 6
    )

    om <- list(
      "biomass" = om_output$biomass.mt,
      "abundance" = om_output$abundance / 1000,
      "ssb" = om_output$SSB,
      "recruit" = om_output$N.age[, 1] / 1000,
      "Ftot" = apply(om_output$FAA, 1, max),
      "landing" = om_output$L.mt$fleet1,
      "survey" = om_output$survey_index$survey1,
      "r0" = om_input$R0 / 1000,
      "q" = om_output$survey_q,
      "selexparm_fleet" = om_input$sel_fleet,
      "selexparm_survey" = om_input$sel_survey,
      "recruit_deviation" = om_input$logR.resid
    )

    mas <- list(
      "biomass" = unlist(pop$undifferentiated$biomass$values),
      "abundance" = unlist(pop$undifferentiated$abundance$values),
      "ssb" = unlist(pop$undifferentiated$spawning_stock_biomass$values),
      "recruit" = unlist(pop$undifferentiated$recruits$values),
      "Ftot" = unlist(pop$undifferentiated$fishing_mortality$values),
      "landing" = unlist(flt$undifferentiated$catch_biomass$values),
      "survey" = unlist(srvy$undifferentiated$survey_biomass$values),
      "r0" = exp(parameter_table$Value[parameter_table$Parameter == "log_R0_1"]),
      "q" = list(parameter_table$Value[parameter_table$Parameter == "q_1"] / 1000),
      selexparm_fleet = list(
        "a50" = parameter_table$Value[parameter_table$Parameter == "logistic_selectivity_a50_1"],
        "slope" = parameter_table$Value[parameter_table$Parameter == "logistic_selectivity_slope_1"]
      ),
      selexparm_survey = list(
        "a50" = parameter_table$Value[parameter_table$Parameter == "logistic_selectivity_a50_2"],
        "slope" = parameter_table$Value[parameter_table$Parameter == "logistic_selectivity_slope_2"]
      )
    )

    # Check if relative errors of key variables are less than 0.15
    var <- c(
      "biomass", "abundance", "ssb", "recruit", "Ftot",
      "landing", "survey"
    )
    for (i in 1:length(var)) {
      x <- mas[[var[i]]]
      y <- om[[var[i]]]
      for (j in 1:length(x)) {
        expect_lt(abs(x[j] - y[j]) / y[j], 0.15) # <15%
      }
    }

    # Check if relative errors of key paramters are less than 0.1 (R0: 2%)
    summary_table <- matrix(c(
      om$r0, mas$r0,
      om$q$survey1, mas$q[[1]],
      om$selexparm_fleet$fleet1$A50.sel1, mas$selexparm_fleet$a50,
      om$selexparm_fleet$fleet1$slope.sel1, 1 / mas$selexparm_fleet$slope,
      om$selexparm_survey$survey1$A50.sel1, mas$selexparm_survey$a50,
      om$selexparm_survey$survey1$slope.sel1, 1 / mas$selexparm_survey$slope
    ),
    ncol = 2, byrow = TRUE
    )
    summary_table <- as.data.frame(summary_table)
    colnames(summary_table) <- c("OM", "MAS")
    rownames(summary_table) <- c(
      "R0", "q",
      "Fleet selectivity A50",
      "Fleet selectivity slope",
      "Survey selectivity A50",
      "Survey selectivity slope"
    )

    for (i in 1:nrow(summary_table)) {
      x <- summary_table$MAS[i]
      y <- summary_table$OM[i]

      if (rownames(summary_table)[i] == "R0") {
        expect_lt(abs(x - y) / y, 0.01) # <1%
      } else {
        expect_equal(x, y, tolerance = 0.1) # <0.1
      }
    }
  }
)

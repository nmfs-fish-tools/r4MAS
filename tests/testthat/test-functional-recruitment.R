
# Test r4MAS: recruitment-functional-test -------------------------------------------------------------------

test_that(
  "r4MAS Beverton-Holt recruit_expect works",
  {
    test_path <- file.path(test_example_path, "functional_test_data")
    source(file.path(test_path, "recruitment.R"))
    mas_recruitment <- c()

    # Load module
    r4mas <- Rcpp::Module("rmas", dyn.load(dll_path))

    for (i in 1:nrow(BevertonHolt_data)) {
      recruitment <- new(r4mas$BevertonHoltRecruitment)

      recruitment$R0$value <- BevertonHolt_data$r0[i]
      recruitment$R0$estimated <- FALSE
      recruitment$h$value <- BevertonHolt_data$h[i]
      recruitment$h$estimated <- FALSE
      recruitment$sigma_r$value <- BevertonHolt_data$sigma_r[i]
      recruitment$sigma_r$estimated <- FALSE
      recruitment$estimate_deviations <- FALSE
      recruitment$SetDeviations(BevertonHolt_data$deviation[i])
      recruitment$use_bias_correction <- FALSE
      mas_recruitment[i] <- recruitment$Evaluate(SB0 = BevertonHolt_data$sb0[i], sb = BevertonHolt_data$ssb[i])
    }

    expect_equal(
      object = mas_recruitment,
      expected = BevertonHolt_data$recruit_expect,
      tolerance = 1
    ) # <1
  }
)

test_that(
  "r4MAS Beverton-Holt recruit_deviation works",
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
    recruitment$R0$estimated <- FALSE
    recruitment$R0$phase <- 1
    recruitment$h$value <- om_input$h
    recruitment$h$estimated <- FALSE
    recruitment$h$phase <- 3
    recruitment$h$min <- 0.2001
    recruitment$h$max <- 1.0
    recruitment$sigma_r$value <- om_input$logR_sd
    recruitment$sigma_r$estimated <- FALSE
    recruitment$sigma_r$min <- 0
    recruitment$sigma_r$max <- 1.0
    recruitment$sigma_r$phase <- 2
    recruitment$estimate_deviations <- FALSE
    recruitment$constrained_deviations <- FALSE
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

    # Maturity
    maturity <- new(r4mas$Maturity)
    maturity$values <- om_input$mat.age * 0.5

    # Natural Mortality
    natural_mortality <- new(r4mas$NaturalMortality)
    natural_mortality$SetValues(om_input$M.age)

    # define Movement (only 1 area in this model)
    movement <- new(r4mas$Movement)
    movement$connectivity_females <- c(0.0)
    movement$connectivity_males <- c(0.0)
    movement$connectivity_recruits <- c(0.0)

    # Initial Deviations
    initial_deviations <- new(r4mas$InitialDeviations)
    initial_deviations$values <- rep(0.0, times = om_input$nages)
    initial_deviations$estimate <- TRUE
    initial_deviations$phase <- 1

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
    fishing_mortality$estimate <- FALSE
    fishing_mortality$phase <- 1
    fishing_mortality$min <- 0.0
    fishing_mortality$max <- 4
    fishing_mortality$SetValues(om_output$f)

    # Selectivity Model
    fleet_selectivity <- new(r4mas$LogisticSelectivity)
    fleet_selectivity$a50$value <- om_input$sel_fleet$fleet1$A50.sel
    fleet_selectivity$a50$estimated <- FALSE
    fleet_selectivity$a50$phase <- 2
    fleet_selectivity$a50$min <- 0.0
    fleet_selectivity$a50$max <- max(om_input$ages)

    fleet_selectivity$slope$value <- 1 / om_input$sel_fleet$fleet1$slope.sel
    fleet_selectivity$slope$estimated <- FALSE
    fleet_selectivity$slope$phase <- 2
    fleet_selectivity$slope$min <- 0
    fleet_selectivity$slope$max <- max(om_input$ages)

    survey_selectivity <- new(r4mas$LogisticSelectivity)
    survey_selectivity$a50$value <- om_input$sel_survey$survey1$A50.sel
    survey_selectivity$a50$estimated <- FALSE
    survey_selectivity$a50$phase <- 2
    survey_selectivity$a50$min <- 0.0
    survey_selectivity$a50$max <- max(om_input$ages)

    survey_selectivity$slope$value <- 1 / om_input$sel_survey$survey1$slope.sel
    survey_selectivity$slope$estimated <- FALSE
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
    survey$q$estimated <- FALSE
    survey$q$phase <- 1

    # build the MAS model
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

    # read output
    args <- commandArgs(trailingOnly = TRUE)
    args <- "test_output.json"
    json_output <- jsonlite::read_json(file.path(test_path, args[1]))
    popdy <- json_output$population_dynamics
    pop <- popdy$populations[[1]]
    flt <- popdy$fleets[[1]]
    srvy <- popdy$surveys[[1]]

    # Check if relative error in recruitment between MAS and OM is less than 0.15 in year 1 and less than 2% from year 2

    object <- unlist(pop$undifferentiated$recruits$values)
    expect <- om_output$N.age[, 1] / 1000

    # Recruitment in year 1
    expect_lt(abs(object[1] - expect[1]) / expect[1], 0.15) # <15%

    # Recruitment from year 2
    for (i in 2:length(object)) {
      expect_lt(abs(object[i] - expect[i]) / expect[i], 0.01) # <1%
    }
  }
)

test_that(
  "r4MAS Beverton-Holt recruitment module estimation function works",
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
    recruitment$h$estimated <- TRUE
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

    # Maturity
    maturity <- new(r4mas$Maturity)
    maturity$values <- om_input$mat.age * 0.5

    # Natural Mortality
    natural_mortality <- new(r4mas$NaturalMortality)
    natural_mortality$SetValues(om_input$M.age)

    # define Movement (only 1 area in this model)
    movement <- new(r4mas$Movement)
    movement$connectivity_females <- c(0.0)
    movement$connectivity_males <- c(0.0)
    movement$connectivity_recruits <- c(0.0)

    # Initial Deviations
    initial_deviations <- new(r4mas$InitialDeviations)
    initial_deviations$values <- rep(0.0, times = om_input$nages)
    initial_deviations$estimate <- TRUE
    initial_deviations$phase <- 1

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
    fishing_mortality$estimate <- FALSE
    fishing_mortality$phase <- 1
    fishing_mortality$min <- 0.0
    fishing_mortality$max <- 4
    fishing_mortality$SetValues(om_output$f)

    # Selectivity Model
    fleet_selectivity <- new(r4mas$LogisticSelectivity)
    fleet_selectivity$a50$value <- om_input$sel_fleet$fleet1$A50.sel
    fleet_selectivity$a50$estimated <- FALSE
    fleet_selectivity$a50$phase <- 2
    fleet_selectivity$a50$min <- 0.0
    fleet_selectivity$a50$max <- max(om_input$ages)

    fleet_selectivity$slope$value <- 1 / om_input$sel_fleet$fleet1$slope.sel
    fleet_selectivity$slope$estimated <- FALSE
    fleet_selectivity$slope$phase <- 2
    fleet_selectivity$slope$min <- 0
    fleet_selectivity$slope$max <- max(om_input$ages)

    survey_selectivity <- new(r4mas$LogisticSelectivity)
    survey_selectivity$a50$value <- om_input$sel_survey$survey1$A50.sel
    survey_selectivity$a50$estimated <- FALSE
    survey_selectivity$a50$phase <- 2
    survey_selectivity$a50$min <- 0.0
    survey_selectivity$a50$max <- max(om_input$ages)

    survey_selectivity$slope$value <- 1 / om_input$sel_survey$survey1$slope.sel
    survey_selectivity$slope$estimated <- FALSE
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
    survey$q$estimated <- FALSE
    survey$q$phase <- 1

    # build the MAS model
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

    # read output
    args <- commandArgs(trailingOnly = TRUE)
    args <- "test_output.json"
    json_output <- jsonlite::read_json(file.path(test_path, args[1]))
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

    # Annual recruitment

    object <- unlist(pop$undifferentiated$recruits$values)
    expect <- om_output$N.age[, 1] / 1000

    for (i in 1:length(object)) {
      expect_lt(abs(object[i] - expect[i]) / expect[i], 0.16) # <16%
    }

    # R0
    object <- exp(parameter_table$Value[parameter_table$Parameter == "log_R0_1"])
    expect_length(object, 1)
    expect_type(object, "double")

    # h
    object <- parameter_table$Value[parameter_table$Parameter == "h1"]
    expect_length(object, 1)
    expect_type(object, "double")

    # sigma_r
    object <- parameter_table$Value[parameter_table$Parameter == "sigma_r1"]
    expect_length(object, 1)
    expect_type(object, "double")
  }
)

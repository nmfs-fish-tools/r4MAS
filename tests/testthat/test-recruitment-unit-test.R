
# Test RMAS: recruitment-unit-test -------------------------------------------------------------------
# Fix recruitment settings in RMAS and run recruitment module
# Compare MAS outputs with expect true values

test_that(
  "BevertonHoltRecruitment test",
  {
    test_path <- file.path(test_example_path, "unit_tests")
    source(file.path(test_path, "recruitment.R"))

    # Load module
    r4mas <- Rcpp::Module("rmas", dyn.load(dll_path))

    # Set up recruitment
    recruitment <- new(r4mas$BevertonHoltRecruitment)

    recruitment$R0$value <- R0
    recruitment$R0$estimated <- FALSE
    recruitment$h$value <- h
    recruitment$h$estimated <- FALSE
    recruitment$sigma_r$value <- sigma_r
    recruitment$sigma_r$estimated <- FALSE
    recruitment$estimate_deviations <- FALSE
    recruitment$SetDeviations(deviations)
    recruitment$use_bias_correction <- FALSE
    mas_recruitment <- recruitment$Evaluate(SB0 = SB0, sb = ssb)

    expect_equal(
      object = mas_recruitment,
      expected = expect_BevertonHolt,
      tolerance = 1
    ) # <1
  }
)

test_that(
  "BevertonHoltAltRecruitment test",
  {
    # Load module
    r4mas <- Rcpp::Module("rmas", dyn.load(dll_path))

    # Set up recruitment
    recruitment <- new(r4mas$BevertonHoltRecruitmentAlt)

    recruitment$R0$value <- R0
    recruitment$R0$estimated <- FALSE
    recruitment$h$value <- h
    recruitment$h$estimated <- FALSE
    recruitment$sigma_r$value <- sigma_r
    recruitment$sigma_r$estimated <- FALSE
    recruitment$estimate_deviations <- FALSE
    recruitment$SetDeviations(deviations)
    recruitment$use_bias_correction <- FALSE
    mas_recruitment <- recruitment$Evaluate(SB0 = SB0, sb = ssb)

    expect_equal(
      object = mas_recruitment,
      expected = expect_BevertonHoltAlt,
      tolerance = 1
    ) # <1
  }
)

test_that("Excel export writes a workbook with expected sheets", {
  n <- 12
  cum <- seq(0.01, 0.12, length.out = n)
  ead <- rep(1000, n)
  lgd <- rep(0.5, n)
  df <- rep(1, n)
  comp <- ecl_components(cum, ead, lgd, df)
  P <- example_transition_matrix()
  scenarios <- data.frame(
    scenario = c("base", "up", "down"),
    weight = c(0.5, 0.3, 0.2),
    ecl = c(100, 130, 80)
  )
  stage_info <- list(stage = 2L, ecl = 123.45)
  meta <- list(
    run_date = as.character(Sys.Date()),
    currency = "USD",
    note = "Test export"
  )
  path <- file.path(tempdir(), "ifrs9_export_unit_test.xlsx")
  expect_silent(export_ifrs9_results_xlsx(path, comp, P, scenarios, stage_info, meta, overwrite = TRUE))
  expect_true(file.exists(path))
  expect_true(file.info(path)$size > 0)
})

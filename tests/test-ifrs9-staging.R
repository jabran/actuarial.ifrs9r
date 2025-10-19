
test_that("Stage 1 uses short horizon and Stage 2/3 uses lifetime", {
  n <- 36
  cum <- seq(0.01, 0.36, length.out = n)
  ead <- rep(1000, n)
  lgd <- rep(0.45, n)
  df <- rep(1, n)
  ecl_12 <- ecl_from_cum_pd(cum[1:12], ead[1:12], lgd[1:12], df[1:12])
  ecl_life <- ecl_from_cum_pd(cum, ead, lgd, df)

  expect_equal(ecl_staged(cum, ead, lgd, df, stage = 1L, short_horizon = 12), ecl_12)
  expect_equal(ecl_staged(cum, ead, lgd, df, stage = 2L), ecl_life)
  expect_equal(ecl_staged(cum, ead, lgd, df, stage = 3L), ecl_life)
})

test_that("SICR staging thresholds behave as expected", {
  expect_equal(stage_from_sicr(0.12, 0.25, credit_impaired = FALSE, threshold_ratio = 2, threshold_abs = 0.1), 2L)
  expect_equal(stage_from_sicr(0.12, 0.15, credit_impaired = FALSE, threshold_ratio = 2, threshold_abs = 0.1), 1L)
  expect_equal(stage_from_sicr(0.00, 0.05, credit_impaired = FALSE, threshold_ratio = 2, threshold_abs = 0.1), 2L) # abs test
  expect_equal(stage_from_sicr(0.10, 0.21, credit_impaired = FALSE, threshold_ratio = 2.0, threshold_abs = 0.15), 2L) # ratio test
  expect_equal(stage_from_sicr(0.10, 0.09, credit_impaired = TRUE), 3L)
})

test_that("ECL positive and increases with LGD", {
  n <- 24
  cum <- seq(0.01, 0.2, length.out = n)
  ead <- rep(1000, n)
  lgd1 <- rep(0.4, n)
  lgd2 <- rep(0.6, n)
  df <- rep(1, n)
  ecl1 <- ecl_from_cum_pd(cum, ead, lgd1, df)
  ecl2 <- ecl_from_cum_pd(cum, ead, lgd2, df)
  expect_true(ecl1 > 0)
  expect_true(ecl2 > ecl1)
})

test_that("Scenario weights must sum to 1 and weighting works", {
  n <- 12
  cp <- list(rep(0.1, n), rep(0.2, n))
  ead <- rep(1000, n)
  lgd <- rep(0.5, n)
  df <- rep(1, n)
  expect_error(weighted_ecl_from_cum_pd(cp, c(0.6, 0.3), ead, lgd, df))
  ecls <- vapply(cp, function(x) ecl_from_cum_pd(x, ead, lgd, df), numeric(1))
  w <- c(0.6, 0.4)
  expect_silent({
    w_ecl <- weighted_ecl_from_cum_pd(cp, w, ead, lgd, df)
    expect_equal(w_ecl, sum(ecls * w))
  })
})

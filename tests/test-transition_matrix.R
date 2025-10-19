test_that("example transition matrix validates and powers correctly", {
  P <- example_transition_matrix()
  expect_silent(tm_validate(P, default_state = "Default"))
  P2 <- tm_k_step(P, 2)
  expect_true(all(abs(rowSums(P2) - 1) < 1e-8))
})

test_that("lifetime PD is non-decreasing and bounded", {
  P <- example_transition_matrix()
  cum <- lifetime_pd_from_state(P, state = "A", horizon = 60)
  expect_true(all(diff(cum) >= -1e-10))
  expect_true(all(cum >= 0 & cum <= 1))
})

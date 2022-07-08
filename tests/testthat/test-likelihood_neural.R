
# test for bad input
test_that("faulty input is effectively handled", {
  # simulated data
  set.seed(2022)
  true <- param_draw(c("a", "b", "t0", "sd"),
                     n_drift = 8,
                     dynamic = FALSE)
  d <- simulate_data(true_pars = true,
                     sigma_gen = 0.01)
  # adjust the passed parameters
  expect_error(likelihood_neural(true[-1],
                                 dataset = d))
  rm(d)
  # work with empirical data
  data("data_neural")
  d_copy <- data_neural
  d_copy$neural <- rnorm(nrow(d_copy))
  d_copy$mean_v1 <- rnorm(nrow(d_copy))
  expect_error(likelihood_neural(true,
                                 dataset = d_copy))
})

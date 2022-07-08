
# test for bad input
test_that("faulty input is effectively handled", {
  # tests with empirical data
  data("data_dynamic")
  d_copy <- data_dynamic
  d_copy$stim <- NULL
  expect_error(simulate_data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy <- data_dynamic
  d_copy$repetition <- NULL
  expect_error(simulate_data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  # tests with empirical data
  data("data_neural")
  d_copy <- data_neural
  d_copy$stim <- NULL
  expect_error(simulate_data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy <- data_neural
  d_copy$repetition <- NULL
  expect_error(simulate_data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy <- data_neural
  d_copy$block_nr <- NULL
  expect_error(simulate_data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
})

# test whether the output we get is expected
test_that("the output is the same as the functions it calls", {
  # dLBA: simulated data
  true <- param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                     n_drift  = NULL,
                     dynamic  = TRUE)
  data_dynamic <- simulate_dynamic(true_pars = true,
                                   sigma_gen = 0.01)
  data_neural <- simulate_data(true_pars = true,
                               sigma_gen = 0.01)
  expect_identical(colnames(data_dynamic), colnames(data_neural))
  # nLBA: simulated data
  true <- param_draw(base_par = c("a", "b", "t0", "sd"),
                     n_drift  = 8,
                     dynamic  = FALSE)
  data_dynamic <- simulate_neural(true_pars = true,
                                  sigma_gen = 0.01)
  data_neural <- simulate_data(true_pars = true,
                               sigma_gen = 0.01)
  expect_identical(colnames(data_dynamic), colnames(data_neural))
})

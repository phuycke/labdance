
# test for bad input
test_that("faulty input is effectively handled", {
  # parameters that can be used to simulate data
  true <- param_draw(base_par = c("a", "b", "t0", "sd"),
                     n_drift  = 8,
                     dynamic  = FALSE)
  # test with simulated data
  expect_error(simulate_neural(true_pars = NULL,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate_neural(sub_id    = -1,
                               true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate_neural(n_blocks  = -1,
                               true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = -6,
                               dataset   = NULL))
  expect_error(simulate_neural(true_pars = seq(.1, .4, .1),
                               sigma_gen = 0.01,
                               dataset   = NULL))
  # tests with empirical data
  data("data_neural")
  d_copy <- data_neural
  d_copy$stim <- NULL
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  d_copy <- data_neural
  d_copy$repetition <- NULL
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  d_copy <- data_neural
  d_copy$block_nr <- NULL
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  d_copy <- data_neural[NULL, ]
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  # test with dynamic parameters
  true <- param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                     n_drift  = NULL,
                     dynamic  = TRUE)
  expect_error(simulate_neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
})

# test whether the output we get is expected
test_that("the output we get is expected", {
  # without neural data
  n_blocks <- 12
  d <- simulate_neural(n_blocks  = n_blocks,
                       true_pars = param_draw(n_drift = 8,
                                              dynamic = FALSE),
                       sigma_gen = NULL)
  expect_equal(nrow(d), n_blocks * 32)
  expect_equal(ncol(d), 6)
  expect_false("neural" %in% names(d))
  # with neural data
  d <- simulate_neural(true_pars = param_draw(n_drift = 8,
                                              dynamic = FALSE),
                      sigma_gen = 0.01)
  expect_equal(nrow(d), 512)
  expect_equal(ncol(d), 7)
  expect_true("neural" %in% names(d))
  # check the output of the simulated data
  expect_true(all(unique(d$stim) == 1:4))
  expect_true(all(unique(d$repetition) == 1:8))
  expect_true(all(unique(d$response) %in% 1:2))
  expect_true(all(d$block_nr[seq(1, 512, 32)] == 1:16))
  expect_true(all(d$rt > 0))
  expect_length(unique(d$sub_id), 1)
})

# test whether the output we get is expected
test_that("the output we get is expected", {
  true <- param_draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = FALSE)
  data("data_neural")
  data_neural$block_nr <- rep(1:8, each = 32)
  sim <- simulate_neural(true_pars = true, dataset = data_neural)
  sim <- sim[1:256, ]
  # check whether they are truly equal
  expect_equal(data_neural$stim, sim$stim)
  expect_equal(data_neural$repetition, sim$repetition)
  expect_equal(data_neural$block_nr, sim$block_nr)
})

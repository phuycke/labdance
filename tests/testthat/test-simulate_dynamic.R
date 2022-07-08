
# test for bad input
test_that("faulty input is effectively handled", {
  # parameters that can be used to simulate data
  true = param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = T)
  # test with simulated data
  expect_error(simulate_dynamic(true_pars = NULL,
                                sigma_gen = 0.01,
                                dataset   = NULL))
  expect_error(simulate_dynamic(n_blocks  = -1,
                                true_pars = true,
                                sigma_gen = 0.01,
                                dataset   = NULL))
  expect_error(simulate_dynamic(true_pars = true,
                                sigma_gen = -6,
                                dataset   = NULL))
  expect_error(simulate_dynamic(true_pars = seq(.1, .4, .1),
                                sigma_gen = 0.01,
                                dataset   = NULL))
  # tests with empirical data
  data("data_dynamic")
  d_copy = data_dynamic
  d_copy$stim = NULL
  expect_error(simulate_dynamic(true_pars = true,
                                sigma_gen = 0.01,
                                dataset   = d_copy))
  d_copy = data_dynamic
  d_copy$condition = NULL
  expect_error(simulate_dynamic(true_pars = true,
                                sigma_gen = 0.01,
                                dataset   = d_copy))
  # test with dynamic parameters
  true = param_draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = F)
  expect_error(simulate_dynamic(true_pars = true,
                                sigma_gen = 0.01,
                                dataset   = NULL))
})

# test whether the output we get is expected
test_that("the output we get is expected", {
  n_blocks = 12
  d = simulate_dynamic(n_blocks  = n_blocks,
                       true_pars = param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                                              n_drift  = NULL,
                                              dynamic  = T),
                       sigma_gen = NULL)
  expect_equal(nrow(d), n_blocks * 32)
  expect_equal(ncol(d), 7)
  expect_false("neural" %in% names(d))
  # with neural data
  d = simulate_dynamic(true_pars = param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                                              n_drift  = NULL,
                                              dynamic  = T),
                       sigma_gen = 0.01)
  expect_equal(nrow(d), 512)
  expect_equal(ncol(d), 8)
  expect_true("neural" %in% names(d))
  # check the output of the simulated data
  expect_true(all(unique(d$stim) == 1:4))
  expect_true(all(unique(d$condition) %in% c("novel", "repeating")))
  expect_true(all(unique(d$response) %in% 1:2))
  expect_true(all(d$block_nr[seq(1, 512, 32)] == 1:16))
  expect_true(all(d$rt > 0))
  expect_true(all(c("mean_v1", "mean_v2") %in% colnames(d)))
  expect_true(all(round(d$mean_v1 + d$mean_v2, .1) == 1))
})

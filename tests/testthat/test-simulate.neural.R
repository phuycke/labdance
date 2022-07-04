
# test for bad input
test_that("param.draw handles faulty input correctly", {
  # parameters that can be used to simulate data
  true = param.draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = F)
  # test with simulated data
  expect_error(simulate.neural(true_pars = NULL,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate.neural(sub_id    = -1,
                               true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate.neural(n_blocks  = -1,
                               true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate.neural(true_pars = true,
                               sigma_gen = -6,
                               dataset   = NULL))
  expect_error(simulate.neural(true_pars = NULL,
                               sigma_gen = 0.01,
                               dataset   = NULL))
  expect_error(simulate.neural(true_pars = seq(.1, .4, .1),
                               sigma_gen = 0.01,
                               dataset   = NULL))
  # tests with empirical data
  load(file = system.file("data", "sub-02 - simulate.neural.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$stim = NULL
  expect_error(simulate.neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  d_copy = d
  d_copy$repetition = NULL
  expect_error(simulate.neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  d_copy = d
  d_copy$block_nr = NULL
  expect_error(simulate.neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = d_copy))
  # test with dynamic parameters
  true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = T)
  expect_error(simulate.neural(true_pars = true,
                               sigma_gen = 0.01,
                               dataset   = NULL))
})


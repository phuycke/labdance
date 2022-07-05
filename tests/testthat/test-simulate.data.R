
# test for bad input
test_that("param.draw handles faulty input correctly", {
  # tests with empirical data
  load(file = system.file("data", "sub-02 - simulate.dynamic.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$stim = NULL
  expect_error(simulate.data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy = d
  d_copy$repetition = NULL
  expect_error(simulate.data(true_pars = true,
                                sigma_gen = 0.01,
                                dataset   = d_copy))
  # tests with empirical data
  load(file = system.file("data", "sub-02 - simulate.neural.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$stim = NULL
  expect_error(simulate.data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy = d
  d_copy$repetition = NULL
  expect_error(simulate.data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
  d_copy = d
  d_copy$block_nr = NULL
  expect_error(simulate.data(true_pars = true,
                             sigma_gen = 0.01,
                             dataset   = d_copy))
})

# test whether the output we get is expected
test_that("the output is the same as the functions it calls", {
  # dLBA: simulated data
  true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = T)
  d1 = simulate.dynamic(true_pars = true,
                        sigma_gen = 0.01)
  d2 = simulate.data(true_pars = true,
                     sigma_gen = 0.01)
  expect_identical(colnames(d1), colnames(d2))
  # nLBA: simulated data
  true = param.draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = F)
  d1 = simulate.neural(true_pars = true,
                       sigma_gen = 0.01)
  d2 = simulate.data(true_pars = true,
                     sigma_gen = 0.01)
  expect_identical(colnames(d1), colnames(d2))
})


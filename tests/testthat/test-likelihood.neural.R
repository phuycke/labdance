
# test for bad input
test_that("faulty input is effectively handled", {
  # simulated data
  set.seed(2022)
  true = param.draw(c("a", "b", "t0", "sd"),
                      n_drift = 8,
                      dynamic = F)
  d = simulate.data(true_pars = true,
                    sigma_gen = 0.01)
  # adjust the passed parameters
  expect_error(likelihood.neural(true[-1],
                                 dataset = d))
  # work with empirical data
  load(file = system.file("data", "simulate.neural.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$neural = rnorm(nrow(d_copy))
  d_copy$mean_v1 = rnorm(nrow(d_copy))
  expect_error(likelihood.neural(true,
                                 dataset = d_copy))
})



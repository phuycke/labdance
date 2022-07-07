
# test for bad input
test_that("faulty input is effectively handled", {
  # simulated data
  set.seed(2022)
  true = param.draw(c("a", "b", "t0", "sd"),
                      n_drift = 8,
                      dynamic = F)
  d = simulate.data(true_pars = true)
  # adjust the passed parameters
  expect_error(likelihood.behavioral(true[-1],
                                     dataset = d))
  # work with empirical data
  load(file = system.file("data", "simulate.neural.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$rt = NULL
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  d_copy$response = NULL
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  names(d_copy) = NULL
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  d_copy$response = rep(c(0, 1), each = (nrow(d_copy) / 2))
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  d_copy$response = rep(c("0", "1"), each = (nrow(d_copy) / 2))
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  d_copy$response = rep(c(2, 3), each = (nrow(d_copy) / 2))
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))
  d_copy = d
  d_copy$response = rep(c(1, 3), each = (nrow(d_copy) / 2))
  expect_error(likelihood.behavioral(true,
                                     dataset = d_copy))

})



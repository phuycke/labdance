
# test for bad input
test_that("faulty input is effectively handled", {
  # simulated data
  set.seed(2022)
  true <- param_draw(c("a", "b", "t0", "sd"),
                     n_drift = 8,
                     dynamic = FALSE)
  d <- simulate_data(true_pars = true)
  # adjust the passed parameters
  expect_error(likelihood_behavioral(true[-1],
                                     dataset = d))
  rm(d)
  # work with empirical data
  data("data_neural")
  d_copy <- data_neural
  d_copy$rt <- NULL
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  d_copy$response <- NULL
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  names(d_copy) <- NULL
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  d_copy$response <- rep(c(0, 1), each = (nrow(d_copy) / 2))
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  d_copy$response <- rep(c("0", "1"), each = (nrow(d_copy) / 2))
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  d_copy$response <- rep(c(2, 3), each = (nrow(d_copy) / 2))
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))
  d_copy <- data_neural
  d_copy$response <- rep(c(1, 3), each = (nrow(d_copy) / 2))
  expect_error(likelihood_behavioral(true,
                                     dataset = d_copy))

})

# test for bad input
test_that("the otuput is a number", {
  # simulated data
  set.seed(2022)
  true <- param_draw(c("a", "b", "t0", "sd"),
                     n_drift = 8,
                     dynamic = FALSE)
  d <- simulate_data(true_pars = true)
  # adjust the passed parameters
  expect_equal(class(likelihood_behavioral(true,
                                           d)),
               "numeric")
  expect_true(likelihood_behavioral(true, d) <= 1e6)
})

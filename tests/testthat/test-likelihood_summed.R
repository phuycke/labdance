
# test for bad input
test_that("the output of likelihood_summed is expected", {
  # simulated data
  set.seed(2022)
  # LBA
  true <- param_draw(base_par = c("a", "b", "t0", "sd"),
                     n_drift  = 8,
                     dynamic  = FALSE)
  simulated <- simulate_data(true_pars = true,
                             dataset   = NULL,
                             sigma_gen = NULL)
  expect_equal(likelihood_summed(to_optim = true,
                                 dataset  = simulated),
               likelihood_behavioral(to_optim = true,
                                     dataset  = simulated))
  # dynamic LBA
  true <- param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                     n_drift  = NULL,
                     dynamic  = TRUE)
  simulated <- simulate_data(true_pars = true,
                            dataset   = NULL,
                            sigma_gen = NULL)
  expect_equal(likelihood_summed(to_optim = true,
                                 dataset  = simulated),
               likelihood_behavioral(to_optim = true,
                                 dataset  = simulated))
  # neural LBA
  true <- param_draw(base_par = c("a", "b", "t0", "sd"),
                     n_drift  = 8,
                     dynamic  = FALSE)
  simulated <- simulate_data(true_pars = true,
                             dataset   = NULL,
                             sigma_gen = 0.01)
  ll_b <- likelihood_behavioral(to_optim = true, dataset  = simulated)
  ll_n <- likelihood_neural(to_optim = true, dataset  = simulated)
  expect_equal(likelihood_summed(to_optim  = true,
                                 dataset   = simulated,
                                 sigma_mod = 0.01),
               ll_b + (1 / (2 * (0.01)^2)) * ll_n)
  # dynamic neural LBA
  true <- param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = TRUE)
  simulated <- simulate_data(true_pars = true,
                             dataset   = NULL,
                             sigma_gen = 0.01)
  ll_b <- likelihood_behavioral(to_optim = true, dataset  = simulated)
  ll_n <- likelihood_neural(to_optim = true, dataset  = simulated)
  expect_equal(likelihood_summed(to_optim  = true,
                                 dataset   = simulated,
                                 sigma_mod = 0.01),
               ll_b + (1 / (2 * (0.01)^2)) * ll_n)
})


# test for bad input
test_that("the output of likelihood.summed is expected", {
  # simulated data
  set.seed(2022)
  # LBA
  true = param.draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = F)
  simulated = simulate.data(true_pars = true,
                            dataset   = NULL,
                            sigma_gen = NULL)
  expect_equal(likelihood.summed(to_optim = true,
                                 dataset  = simulated),
               likelihood.behavioral(to_optim = true,
                                     dataset  = simulated))
  # dynamic LBA
  true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = T)
  simulated = simulate.data(true_pars = true,
                            dataset   = NULL,
                            sigma_gen = NULL)
  expect_equal(likelihood.summed(to_optim = true,
                                 dataset  = simulated),
               likelihood.behavioral(to_optim = true,
                                 dataset  = simulated))
  # neural LBA
  true = param.draw(base_par = c("a", "b", "t0", "sd"),
                    n_drift  = 8,
                    dynamic  = F)
  simulated = simulate.data(true_pars = true,
                            dataset   = NULL,
                            sigma_gen = 0.01)
  ll.b = likelihood.behavioral(to_optim = true, dataset  = simulated)
  ll.n = likelihood.neural(to_optim = true, dataset  = simulated)
  expect_equal(likelihood.summed(to_optim  = true,
                                 dataset   = simulated,
                                 sigma_mod = 0.01),
               ll.b + (1/(2*(0.01)^2)) * ll.n)
  # dynamic neural LBA
  true = param.draw(base_par = c("a", "b", "t0", "sd", "beta"),
                    n_drift  = NULL,
                    dynamic  = T)
  simulated = simulate.data(true_pars = true,
                            dataset   = NULL,
                            sigma_gen = 0.01)
  ll.b = likelihood.behavioral(to_optim = true, dataset  = simulated)
  ll.n = likelihood.neural(to_optim = true, dataset  = simulated)
  expect_equal(likelihood.summed(to_optim  = true,
                                 dataset   = simulated,
                                 sigma_mod = 0.01),
               ll.b + (1/(2*(0.01)^2)) * ll.n)
})



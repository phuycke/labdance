
# test for bad input
test_that("param.draw handles faulty input correctly", {
  # test beta
  expect_error(netinputs(beta = -.5))
  expect_error(netinputs(beta = "0.5"))
  expect_error(netinputs(beta = 100))
  expect_error(netinputs(beta = NULL))
  # tests with empirical data
  load(file = system.file("data", "sub-02 - simulate.dynamic.RData",
                          package = "labdance"))
  d_copy = d
  d_copy$stim = NULL
  expect_error(netinputs(beta    = .5,
                         dataset = d_copy))
  d_copy = d
  d_copy$condition = NULL
  expect_error(netinputs(beta    = .5,
                         dataset = d_copy))
  # test integrity of output
  test = netinputs(beta = .7)
  expect_true(all(round(test[[1]] + test[[2]], 10) == 1))
  test = netinputs(beta = .7,
                   dataset = d)
  expect_true(all(round(test[[1]] + test[[2]], 10) == 1))
})



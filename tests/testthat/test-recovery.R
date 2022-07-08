
# test for bad input
test_that("the output of likelihood.summed is expected", {
  expect_error(recovery(base_par = c("a", "b", "t0", "sd"),
                        cycles = -2))
  expect_error(recovery(base_par = c("a", "b", "t0", "sd"),
                        cycles = "500"))
  expect_error(recovery(base_par = c("a", "b", "t0", "sd"),
                        cycles = 500,
                        sigma_mod = -5))
  expect_error(recovery(base_par = c("a", "b", "t0", "sd"),
                        cycles = 500,
                        sigma_mod = ".2"))
})

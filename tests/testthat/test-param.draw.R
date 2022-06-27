
# check how param.draw reacts on faulty input
test_that("param.draw handles faulty input correctly", {
  expect_error(param.draw(base_par = c(1, "a", "b"),
                          n_drift = 8,
                          dynamic = F))
  expect_error(param.draw(base_par = c("a", "b", "standard error"),
                          n_drift = 8,
                          dynamic = F))
  expect_error(param.draw(base_par = c(),
                          n_drift = 8,
                          dynamic = F))
  expect_error(param.draw(base_par = c("a", "a", "a"),
                          n_drift = 8,
                          dynamic = F))
  expect_error(param.draw(n_drift = 8,
                          dynamic = T))
  expect_error(param.draw(n_drift = NULL,
                          dynamic = F))
  expect_error(param.draw(n_drift = "18",
                          dynamic = F))
  expect_error(param.draw(n_drift = 8,
                          dynamic = "yes"))
  expect_error(param.draw(base_par = c("a", "b", "beta"),
                          n_drift = 8,
                          dynamic = F))
})

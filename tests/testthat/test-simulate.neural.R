
# check how param.draw reacts on faulty input
test_that("param.draw handles faulty input correctly", {
  expect_error(param.draw(base_par = c(1, "a", "b"),
                          n_drift = 8,
                          dynamic = F))
})


# check how param_draw reacts on faulty input
test_that("faulty input is effectively handled", {
  expect_error(param_draw(base_par = c(1, "a", "b"),
                          n_drift = 8,
                          dynamic = FALSE))
  expect_error(param_draw(base_par = c("a", "b", "standard error"),
                          n_drift = 8,
                          dynamic = FALSE))
  expect_error(param_draw(base_par = c(),
                          n_drift = 8,
                          dynamic = FALSE))
  expect_error(param_draw(base_par = c("a", "a", "a"),
                          n_drift = 8,
                          dynamic = FALSE))
  expect_error(param_draw(n_drift = 8,
                          dynamic = TRUE))
  expect_error(param_draw(n_drift = NULL,
                          dynamic = FALSE))
  expect_error(param_draw(n_drift = "18",
                          dynamic = FALSE))
  expect_error(param_draw(n_drift = 8,
                          dynamic = "yes"))
  expect_error(param_draw(base_par = c("a", "b", "beta"),
                          n_drift = 8,
                          dynamic = FALSE))
})


# generate input for the four different models
test_that("the functions provides decent input for each of the four models", {
  # two cases for the simple LBA / nLBA
  expect_length(param_draw(base_par = c("a", "b", "t0"),
                           n_drift = 8,
                           dynamic = FALSE),
                11)
  expect_length(param_draw(n_drift  = 4,
                           dynamic  = FALSE),
                8)
  # three cases for the dLBA / dnLBA
  expect_length(param_draw(base_par = c("a"),
                           dynamic = TRUE),
                1)
  expect_length(param_draw(dynamic = TRUE),
                4)
  expect_length(param_draw(base_par = c("a", "b", "t0", "sd", "beta"),
                           dynamic = TRUE),
                5)

})

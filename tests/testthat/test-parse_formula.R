test_that("parse_formula works", {
  test_data <- data.frame(time = c(1, 2, 3), status = c(1, 0, 1), treat = c("A", "B", "A"), center = c(1, 2, 1))
  
  # without random effects
  res_sep <- parse_formula(formula = "Surv(time, status) ~ treat", data = test_data, family = "exp")
  expect_equal(res_sep$fe_nvars, 1)
  expect_equal(res_sep$fe_vars, "treat")
  expect_null(res_sep$bars)
  
  # with random effects
  res_hier <- parse_formula(formula = "~ treat + (1 | center)", data = test_data)
  expect_equal(res_hier$re_group_var, "center")
  expect_equal(res_hier$fe_nvars, 1)
  expect_equal(res_hier$fe_vars, "treat")
})

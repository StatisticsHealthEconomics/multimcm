test_that("validate_distns works correctly", {
  expect_equal(validate_distns("exp"), "exp")
  expect_equal(validate_distns("weibull"), "weibull")
  expect_equal(validate_distns(c("exp", "lognormal")), c("exp", "lognormal"))
  expect_error(validate_distns("unknown"), "distribution not available")
})

test_that("is_x functions work", {
  pooled_form <- list(nvars = 1, cf_idx = 1, fe_nvars = 1, bars = NULL)
  sep_form <- list(nvars = 2, cf_idx = 2, fe_nvars = 2, bars = NULL)
  hier_form <- list(nvars = 2, cf_idx = 3, fe_nvars = 2, bars = list("group"))

  expect_true(is_pooled(pooled_form))
  expect_false(is_separate(pooled_form))
  
  expect_true(is_separate(sep_form))
  expect_false(is_pooled(sep_form))
  
  expect_true(is_hier(hier_form))
  expect_false(is_hier(sep_form))
  
  expect_true(is_pooled_cf(pooled_form))
  expect_true(is_separate_cf(sep_form))
  expect_true(is_hier_cf(hier_form))
})

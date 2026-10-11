test_that("validate_distns works correctly", {
  expect_equal(validate_distns("exp"), "exp")
  expect_equal(validate_distns("exponential"), "exp")
  expect_equal(validate_distns("expon"), "exp")
  expect_equal(validate_distns("ex"), "exp")
  expect_equal(validate_distns("e"), "exp")
  expect_equal(validate_distns("weibull"), "weibull")
  expect_equal(validate_distns("wei"), "weibull")
  expect_equal(validate_distns("gompertz"), "gompertz")
  expect_equal(validate_distns("gomp"), "gompertz")
  expect_equal(validate_distns("loglogistic"), "loglogistic")
  expect_equal(validate_distns("loglog"), "loglogistic")
  expect_equal(validate_distns("llogis"), "loglogistic")
  expect_equal(validate_distns("lognormal"), "lognormal")
  expect_equal(validate_distns("lognorm"), "lognormal")
  expect_equal(validate_distns("lnorm"), "lognormal")
  expect_equal(validate_distns("gengamma"), "gengamma")
  expect_equal(validate_distns("gengam"), "gengamma")
  expect_equal(validate_distns(c("exponential", "weibull")), c("exp", "weibull"))
  expect_equal(validate_distns(c("exp", "lognorm")), c("exp", "lognormal"))
  expect_error(validate_distns("unknown"), "distribution not available")
  expect_error(validate_distns("log"), "Ambiguous distribution")
})

test_that("create_stancode and create_stancode_postpred handle exp and exponential aliases", {
  expect_no_error(create_stancode(c("exponential", "exponential")))
  expect_no_error(create_stancode_postpred("exponential"))
  expect_no_error(create_stancode_postpred("exp"))
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

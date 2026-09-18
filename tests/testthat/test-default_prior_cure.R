test_that("default_prior_cure works", {
  formula_cure_sep <- list(fe_nlevels = c(2, 2), n_groups = 2, nvars = 1, cf_idx = 2)
  prior_sep <- default_prior_cure(formula_cure_sep, bg_model = 2)
  expect_true("mu_alpha_1" %in% names(prior_sep))
  
  formula_cure_hier <- list(fe_nlevels = c(2), n_groups = 2, nvars = 1, cf_idx = 3)
  prior_hier <- default_prior_cure(formula_cure_hier, bg_model = 1)
  expect_true("mu_sd_cf" %in% names(prior_hier))
})

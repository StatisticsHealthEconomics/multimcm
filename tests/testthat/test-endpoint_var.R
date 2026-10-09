test_that("bmcm_stan parses endpoint_var correctly and falls back smoothly for single arm", {
  
  # Create dummy data
  set.seed(123)
  single_arm_data <- data.frame(
    os = rexp(100, 0.1),
    os_event = sample(0:1, 100, replace=TRUE),
    endpoint = sample(1:2, 100, replace=TRUE),
    dummy_tx = rep("A", 100),
    rate = 0.01
  )

  # Test 1: Single arm, shared cure fraction across multiple endpoints
  out1 <- bmcm_stan(
    input_data = single_arm_data,
    formula = "Surv(time=os, event=os_event) ~ 1",
    cureformula = ~ 1,
    endpoint_var = "endpoint",
    family_latent = "exponential",
    bg_model = "bg_fixed",
    bg_varname = "rate",
    t_max = 400,
    chains=1, iter=10
  )
  
  expect_equal(out1$formula$cure$cf_idx, 1L) # Should be pooled (shared)
  expect_equal(out1$formula$cure$n_groups, 2L)
  expect_equal(out1$formula$cure$group_var, "endpoint")
  expect_equal(out1$formula$cure$tx_var, NA_character_)

  # Test 2: Single arm, separate cure fraction per endpoint
  out2 <- bmcm_stan(
    input_data = single_arm_data,
    formula = "Surv(time=os, event=os_event) ~ 1",
    cureformula = ~ endpoint,
    endpoint_var = "endpoint",
    family_latent = "exponential",
    bg_model = "bg_fixed",
    bg_varname = "rate",
    t_max = 400,
    chains=1, iter=10
  )
  
  expect_equal(out2$formula$cure$cf_idx, 2L) # Should be separate
  expect_equal(out2$formula$cure$n_groups, 2L)
  expect_equal(out2$formula$cure$tx_var, NA_character_)

  # Test 3: Multiple arms, separate cure fraction per endpoint
  multi_arm_data <- single_arm_data
  multi_arm_data$dummy_tx <- sample(c("A", "B"), 100, replace=TRUE)
  
  out3 <- bmcm_stan(
    input_data = multi_arm_data,
    formula = "Surv(time=os, event=os_event) ~ 1",
    cureformula = ~ dummy_tx + endpoint,
    endpoint_var = "endpoint",
    family_latent = "exponential",
    bg_model = "bg_fixed",
    bg_varname = "rate",
    t_max = 400,
    chains=1, iter=10
  )
  
  expect_equal(out3$formula$cure$cf_idx, 2L) # Separate
  expect_equal(out3$formula$cure$tx_var, "dummy_tx")
})


devtools::load_all()

# Create dummy data
set.seed(123)
single_arm_data <- data.frame(
  os = rexp(100, 0.1),
  os_event = sample(0:1, 100, replace=TRUE),
  center_id = sample(1:5, 100, replace=TRUE),
  rate = 0.01
)

out <- bmcm_stan(
  input_data = single_arm_data,
  formula = "Surv(time=os, event=os_event) ~ 1",
  cureformula = ~ 1,
  family_latent = "exponential",
  bg_model = "bg_fixed",
  bg_varname = "rate",
  t_max = 400,
  chains=1, iter=10
)
print("SUCCESS!")


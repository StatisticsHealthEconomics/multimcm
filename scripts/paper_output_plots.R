
# paper output plots
#


# marginal model ----
# cure fraction

# xx <- extract(out)
# hist(xx$curefrac,
#      breaks = 30,
#      freq = FALSE,
#      xlim = c(0,1),
#      xlab = "probability",
#      main = "cure fraction")
# lines(density(xx$pmean_cf), col = "red")
# lines(density(xx$curefrac), col = "blue")

source("R/plot_S_grid.R")

p <-
  plot_S_grid(distns = c("exp", "weibull", "lognormal", "gompertz", "loglogistic"),
              folder = "data/independent/cf hier/bg_fixed_hr1",
              trt = "NIVOLUMAB+IPILIMUMAB",
              data = surv_input_data)

ggsave(p, filename = "plots/plot_S_grid.png", units="in", width=16, height=13, dpi=300)





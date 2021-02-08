
# MCMC diagnostic plots ----

library(bayesplot)
library(survival)

# mcmc_violin(out)
# bayesplot::mcmc_dens_overlay(out)

mcmc_dens(
  out,
  pars = c("mean_pfs","mean_os","mean_bg")) +
  xlim(0,0.25)

mcmc_dens(
  out,
  pars = c("pcurefrac", "curefrac")) +
  xlim(0,1)


mcmc_intervals(out,
  pars = c("pcurefrac", "curefrac"))

mcmc_intervals(out,
  pars = c("mean_pfs","mean_os","mean_bg"))


color_scheme_set("red")
mcmc_pairs(out,
           pars = c("beta_os[1]","beta_pfs[1]","beta_bg[1]",
                    "beta_os[2]","beta_pfs[2]","beta_bg[2]","beta_joint","curefrac"))

mcmc_trace(out,
           pars = c("beta_os[1]","beta_pfs[1]","beta_bg[1]",
                    "beta_os[2]","beta_pfs[2]","beta_bg[2]","beta_joint","curefrac"))


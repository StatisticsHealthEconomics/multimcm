
# posterior parameter statistics


source("R/params_tableTx.R")


################
# hierarchical #
################

coeff_latex <-
  list("beta_os" = c("$\beta_{os, 0}$",
                     "$\beta_{os, 1}$"),
       "beta_pfs" = c("$\beta_{pfs, 0}$",
                      "$\beta_{pfs, 1}$"),
       "cf_os" = c('$\\pi_{os, 1}$',
                   "$\\pi_{os, 2}$",
                   "$\\pi_{os, 3}$"),
       "cf_pfs" = c("$\\pi_{pfs, 1}$",
                    "$\\pi_{pfs, 2}$",
                    "$\\pi_{pfs, 3}$"),
       "cf_global" = c("$\\pi_{global, 1}$",
                       "$\\pi_{global, 2}$",
                       "$\\pi_{global, 3}$"),
       "alpha" = c("$\beta^{\\pi}_1$",
                   "$\beta^{\\pi}_2$",
                   "$\beta^{\\pi}_3$"),
       "sd_cf" = c("$\sigma_1$",
                   "$\sigma_2$",
                   "$\sigma_3$"))

t1 <-
  params_tableTx(
    data_dir = "data/independent/cf hier/bg_fixed_hr1",
    os_distn = "weibull",
    pfs_distn = "exp",
    param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "cf_global", "alpha", "sd_cf"),
    #shape_os, shape_pfs # if present
    coeff_rename = coeff_latex)

t2 <-
  params_tableTx(
    data_dir = "data/independent/cf hier/bg_fixed_hr1",
    os_distn = "exp",
    pfs_distn = "exp",
    param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "cf_global", "alpha", "sd_cf"),
    coeff_rename = coeff_latex)

merge(t1, t2, by = "variable") %>%
  knitr::kable(format = "latex")


############
# separate #
############

coeff_latex <-
  list("beta_os" = c("$\\beta^{os}_0$",
                     "$\\beta^{os}_1$"),
       "beta_pfs" = c("$\\beta^{pfs}_0$",
                      "$\\beta^{pfs}_1$"),
       "cf_os" = c('$\\pi^{os}_1$',
                   "$\\pi^{os}_2$",
                   "$\\pi^{os}_3$"),
       "cf_pfs" = c("$\\pi^{pfs}_1$",
                    "$\\pi^{pfs}_2$",
                    "$\\pi^{pfs}_3$"),
       "alpha_os" = c("$\\beta^{\\pi}_{os, 1}$",
                      "$\\beta^{\\pi}_{os, 2}$",
                      "$\\beta^{\\pi}_{os, 3}$"),
       "alpha_pfs" = c("$\\beta^{\\pi}_{pfs, 1}$",
                       "$\\beta^{\\pi}_{pfs, 2}$",
                       "$\\beta^{\\pi}_{pfs, 3}$"))

t1 <-
  params_tableTx(
    data_dir = "data/independent/cf separate/bg_fixed_hr1",
    os_distn = "exp",
    pfs_distn = "exp",
    param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "alpha_os", "alpha_pfs"),
    coeff_rename = coeff_latex)

t2 <-
  params_tableTx(
    data_dir = "data/independent/cf separate/bg_fixed_hr1",
    os_distn = "lognormal",
    pfs_distn = "lognormal",
    param_names = c("beta_os", "beta_pfs", "cf_os", "cf_pfs", "alpha_os", "alpha_pfs"),
    coeff_rename = coeff_latex)

merge(t1, t2, by = "variable") %>%
  tibble::add_column(` ` = "", .after = 3) %>%
  knitr::kable(format = "latex", escape = FALSE)



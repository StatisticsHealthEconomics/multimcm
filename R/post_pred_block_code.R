# posterior predictions Stan block code -----------------------------------


#' ppv_gen_quants_block("weibull", "weibull")
#'
ppv_gen_quants_block <- function(os_model, pfs_model) {

  distn_params <-
    list(exponential = "lambda",
         weibull = c("shape", "lambda"),
         gompertz = c("shape", "lambda"),
         loglogistic = c("shape", "lambda"),
         lognormal = c("mu", "sd"),          ##TODO: no lambda
         gengamma = "")

  os_params <- paste0(distn_params[[os_model]], "_os")
  pfs_params <- paste0(distn_params[[pfs_model]], "_pfs")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i, ]")

  pfs_params[grep("lambda", pfs_params)] <-
    paste0(pfs_params[grep("lambda", pfs_params)], "[i, ]")

  os_params[!grepl("lambda", os_params)] <-
    paste0(os_params[!grepl("lambda", os_params)], "[i]")

  pfs_params[!grepl("lambda", pfs_params)] <-
    paste0(pfs_params[!grepl("lambda", pfs_params)], "[i]")

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  scode <-
    glue::glue("
    // explicitly loop over samples
    matrix[n_samples, N_os] t_os_tilde;
    matrix[n_samples, N_pfs] t_pfs_tilde;

    for (i in 1:n_samples) {{
      t_os_tilde[i, ] = os_casemix_rng(cf_os[i, ], {os_params}, lambda_os_bg[i, ]);
      t_pfs_tilde[i, ] = pfs_casemix_rng(cf_pfs[i, ], {pfs_params}, lambda_pfs_bg[i, ]);
    }\n\n")

  scode
}


#
ppv_data_block <- function(os_model, pfs_model) {

  ancil_params_os <-
    if (os_model %in% c("gompertz", "loglogistic", "weibull")) {
      "vector[n_samples] shape_os;\n"
    } else {""}

  ancil_params_pfs <-
    if (pfs_model %in% c("gompertz", "loglogistic", "weibull")) {
      "vector[n_samples] shape_pfs;\n"
    } else {""}

  scode <-
    glue::glue("
    int<lower=0> N_os;         // total number of observations
    int<lower=0> N_pfs;

    int<lower=1> n_samples;
    matrix[n_samples, N_os] cf_os;
    matrix[n_samples, N_pfs] cf_pfs;

    matrix[n_samples, N_os] lambda_os;
    matrix[n_samples, N_pfs] lambda_pfs;

    matrix[n_samples, N_os] lambda_os_bg;
    matrix[n_samples, N_pfs] lambda_pfs_bg;\n",
               ancil_params_os,
               ancil_params_pfs)

  scode
}


#
ppv_functions_block<- function(os_model, pfs_model) {

  distn_params <-
    list(exponential = "lambda0[i]",
         weibull = c("shape", "lambda0[i]"),
         gompertz = c("shape", "lambda0[i]"),
         loglogistic = c("shape", "lambda0[i]"),
         lognormal = c("mu0[i]", "sd"),          ##TODO: no lambda
         gengamma = "")

  os_params <- distn_params[[os_model]]
  pfs_params <- distn_params[[pfs_model]]

  pfs_params <- paste(pfs_params, collapse = ", ")
  os_params <- paste(os_params, collapse = ", ")

  input_params_os <-
    if (os_model %in% c("gompertz", "loglogistic", "weibull")) {
      "real shape, row_vector lambda0"
    } else {
      "row_vector lambda0"}

  input_params_pfs <-
    if (pfs_model %in% c("gompertz", "loglogistic", "weibull")) {
      "real shape, row_vector lambda0"
    } else {
      "row_vector lambda0"}

  scode <-
    glue::glue("
    // quantile
    real inv_pgompertz(real p, real shape, real scale) {{
      real res;
      res = 1/shape * log(1 - (log(1-p) * shape/scale));
      return res;
    }

    // random number generator
    real gompertz_rng(real shape, real scale) {{
      real time;
      real prob;
      real asymp;

      prob = uniform_rng(0, 1);
      asymp = 1 - exp(scale/shape);

      if (shape < 0 && prob > asymp) {{
        time = 100;                     // should be inf
      } else {{
        time = inv_pgompertz(prob, shape, scale);
      }
      return time;
    }

    row_vector os_casemix_rng(row_vector curefrac, {input_params_os}, row_vector lambda_bg) {{

      int n = num_elements(curefrac);
      row_vector[n] time;
      real cf[n];

      for (i in 1:n) {{
        cf[i] = uniform_rng(0, 1);

        if (cf[i] < curefrac[i]) {{
          time[i] = exponential_rng(lambda_bg[i]);
        } else {{
          time[i] = fmin({os_model}_rng({os_params}), exponential_rng(lambda_bg[i]));
        }
      }
      return(time);
      }\n\n
    row_vector pfs_casemix_rng(row_vector curefrac, {input_params_pfs}, row_vector lambda_bg) {{

      int n = num_elements(curefrac);
      row_vector[n] time;
      real cf[n];

      for (i in 1:n) {{
        cf[i] = uniform_rng(0, 1);

        if (cf[i] < curefrac[i]) {{
          time[i] = exponential_rng(lambda_bg[i]);
        } else {{
          time[i] = fmin({pfs_model}_rng({pfs_params}), exponential_rng(lambda_bg[i]));
        }
      }
      return(time);
    }\n\n"
    )

  scode
}


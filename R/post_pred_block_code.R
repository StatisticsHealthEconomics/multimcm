# posterior predictions Stan block code -----------------------------------


#' ppv_gen_quants_block("weibull")
#' @keywords internal
#'
ppv_gen_quants_block <- function(model, id = 1) {

  os_params <- glue("{distn_params(model)}_{id]")

  os_params[grep("lambda", os_params)] <-
    paste0(os_params[grep("lambda", os_params)], "[i, ]")

  os_params[!grepl("lambda", os_params)] <-
    paste0(os_params[!grepl("lambda", os_params)], "[i]")

  os_params <- paste(os_params, collapse = ", ")

  scode <-
    glue::glue("
    // explicitly loop over samples
    matrix[n_samples, N_{id}] t_{id}_tilde;

    for (i in 1:n_samples) {{
      t_{id}_tilde[i, ] = {id}_casemix_rng(cf_{id}[i, ], {os_params}, lambda_{id}_bg[i, ]);
    }\n\n")

  scode
}


#
ppv_data_block <- function(model) {

  ancil_params_os <-
    if (model %in% c("gompertz", "loglogistic", "weibull")) {
      "vector[n_samples] shape_{id};\n"
    } else {""}

  scode <-
    glue::glue("
    int<lower=0> N_{id};         // total number of observations
    int<lower=1> n_samples;
    matrix[n_samples, N_{id}] cf_{id};
    matrix[n_samples, N_{id}] lambda_{id};
    matrix[n_samples, N_{id}] lambda_{id}_bg;\n",
               ancil_params_os)

  scode
}


#
ppv_functions_block<- function(model) {

  os_params <- distn_params(model)
  os_params <- paste(os_params, collapse = ", ")

  input_params_os <-
    if (os_model %in% c("gompertz", "loglogistic", "weibull")) {
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

    row_vector {id}_casemix_rng(row_vector curefrac, {input_params_os}, row_vector lambda_bg) {{

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
      }\n\n"
    )

  scode
}


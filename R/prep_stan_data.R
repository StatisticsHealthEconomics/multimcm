
#' prep_stan_data
#'
#' @param input_data
#' @param event_type
#' @param tx_name
#' @param centre_age
#' @param mean_cf
#' @param var_cf
#' @param mu_cf linear regression on cure fraction
#' @param sigma_cf
#' @param mu_bg
#' @param sigma_bg
#'
#' @return List
#' @export
#'
prep_stan_data <- function(input_data,
                           event_type,
                           tx_name,
                           centre_age,
                           mean_cf = NA,
                           var_cf = NA,
                           mu_cf = NA,
                           sigma_cf = NA,
                           mu_bg = c(-8.5, 0.03),
                           sigma_bg = c(1,1)) {

  event_type <- match.arg(arg = event_type, c("PFS", "OS"))
  tx_name <- match.arg(arg = tx_name,
                       c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB"))

  # cure fraction parameters
  cf_params <-
    if (!is.na(mean_cf) && !is.na(var_cf)) {
      mombeta <- MoM_beta(mean_cf, var_cf)
      list(a_cf = mombeta$a,
           b_cf = mombeta$b)
    } else if (all(!is.na(mu_cf)) &&
               all(!is.na(sigma_cf))) {
      list(mu_cf = mu_cf,
           sigma_cf = sigma_cf)
    } else {
      list(a_cf = 1, b_cf = 1)}

  if (event_type == "PFS") {

    tx_dat <-
      input_data %>%
      select(TRTA, pfs, pfs_event, PFSage, PFS_rate) %>%
      mutate(PFS_rate =
               ifelse(PFS_rate == 0, 0.00001, PFS_rate)) %>% # replace 0
      split(input_data$TRTA)
  } else if (event_type == "OS") {

    tx_dat <-
      input_data %>%
      select(TRTA, os, os_event, OSage, OS_rate) %>%
      mutate(OS_rate =
               ifelse(OS_rate == 0, 0.00001, OS_rate)) %>%
      split(input_data$TRTA)
  }

  # centring
  age_adj <- ifelse(centre_age, mean(tx_dat[[tx_name]][[4]]), 0)

  c(cf_params,
    list(
      n = nrow(tx_dat[[tx_name]]),
      t = tx_dat[[tx_name]][[2]],
      d = tx_dat[[tx_name]][[3]],
      H = 2,
      X = matrix(c(rep(1, nrow(tx_dat[[tx_name]])),
                   tx_dat[[tx_name]][[4]] - age_adj),
                 byrow = FALSE,
                 ncol = 2),
      t_max = 60,
      mu_bg = mu_bg,
      sigma_bg = sigma_bg
      # h_bg = tx_dat[[tx_name]][[5]]
    ))
}


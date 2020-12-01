
#' prep_stan_data
#'
#' Data specific to OS or PFS for Stan input.
#'
#' @param input_data surv_input_data
#' @param event_type PFS, OS
#' @param tx_name IPILIMUMAB, NIVOLUMAB, NIVOLUMAB+IPILIMUMAB
#' @param centre_age Logical
#'
#' @return List;
#'         sample size,
#'         times,
#'         censoring indicator,
#'         number of covariates,
#'         covariates
#' @import dplyr
#' @export
#'
prep_stan_data <- function(input_data,
                           event_type,
                           tx_name,
                           centre_age) {

  event_type <- match.arg(arg = event_type, c("PFS", "OS"))
  tx_name <- match.arg(arg = tx_name,
                       c("IPILIMUMAB", "NIVOLUMAB", "NIVOLUMAB+IPILIMUMAB"))

  if (event_type == "PFS") {

    tx_dat <-
      input_data %>%
      select(TRTA, pfs, pfs_event, PFSage, PFS_rate) %>%
      mutate(PFS_rate =
               ifelse(PFS_rate == 0, 0.00001, PFS_rate)) %>% # replace so >0
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

    list(
      n = nrow(tx_dat[[tx_name]]),
      t = tx_dat[[tx_name]][[2]],
      d = tx_dat[[tx_name]][[3]],
      H = 2,
      X = matrix(c(rep(1, nrow(tx_dat[[tx_name]])),
                   tx_dat[[tx_name]][[4]] - age_adj),
                 byrow = FALSE,
                 ncol = 2)
      # h_bg = tx_dat[[tx_name]][[5]]
    )
}


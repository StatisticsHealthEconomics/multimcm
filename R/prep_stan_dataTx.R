
#' prep_stan_dataTx
#'
#' Data specific to OS or PFS for Stan input.
#'
#' @param input_data surv_input_data
#' @param event_type PFS, OS
#' @param centre_age Logical
#' @param bg_model Background model. 1: Exponential distribution; 2: fixed point values from life-table
#' @param bg_hr background all-cause mortality hazard ratio
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
prep_stan_dataTx <- function(input_data,
                             event_type,
                             centre_age,
                             bg_model = 1,
                             bg_hr = 1) {
  
  event_type <- match.arg(arg = event_type, c("PFS", "OS"))
  
  if (event_type == "PFS") {
    
    tx_dat <-
      input_data %>%
      select(TRTA, pfs, pfs_event, PFSage, PFS_rate) %>%
      mutate(PFS_rate =
               ifelse(PFS_rate == 0, 0.00001, PFS_rate)) # replace so >0
    
  } else if (event_type == "OS") {
    
    tx_dat <-
      input_data %>%
      select(TRTA, os, os_event, OSage, OS_rate) %>%
      mutate(OS_rate =
               ifelse(OS_rate == 0, 0.00001, OS_rate))
  }
  
  # centering
  age_adj <- ifelse(centre_age, mean(tx_dat[[4]]), 0)
  
  X_Tx <- 
    as.data.frame(
      matrix(c(rep(1, nrow(tx_dat)),
               tx_dat[[4]] - age_adj),
             byrow = FALSE,
             ncol = 2))
  
  # background hazard point values
  h_bg <-
    if (bg_model == 2) {
      cbind.fill(split(tx_dat[[5]]*bg_hr, tx_dat$TRTA))
    } else {
      numeric(0)}
  
  list(
    n = c(table(surv_input_data$TRTA)),
    t = cbind.fill(split(tx_dat[[2]], tx_dat$TRTA)),
    d = cbind.fill(split(tx_dat[[3]], tx_dat$TRTA)),
    H = 2,
    X = abind.fill(split(X_Tx, tx_dat$TRTA)),
    h_bg = h_bg)
}


# fill in unequal treatment groups with NAs
#
cbind.fill <- function(nm){
  # nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n - nrow(x), ncol(x))))) %>% 
    `colnames<-`(names(nm))
}

#
abind.fill <- function(nm){
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  abind(along = 3, 
        lapply(nm, function (x) 
          rbind(x, matrix(, n - nrow(x), ncol(x)))))
}



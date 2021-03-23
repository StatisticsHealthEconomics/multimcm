
#' @name bind.fill
NULL


#' fill in unequal treatment groups with NAs
#'
#' @rdname bind.fill
cbind.fill <- function(nm){
  # nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n - nrow(x), ncol(x))))) %>% 
    `colnames<-`(names(nm))
}


#'
#' @rdname bind.fill
abind.fill <- function(nm){
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  abind(along = 3, 
        lapply(nm, function (x) 
          rbind(x, matrix(, n - nrow(x), ncol(x)))))
}


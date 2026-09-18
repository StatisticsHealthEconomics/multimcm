#' @importFrom stats as.formula formula model.frame quantile reformulate setNames terms update.formula
#' @importFrom utils modifyList
NULL

utils::globalVariables(c(
  "cf", "tx", "cpt", "treatment", "mean", "event",
  "Distribution", "Cut-point", "2.5%", "97.5%",
  "time", "type", "value", "Tx", "type_tx",
  "scenario", "endpoint", "lower", "upper",
  "distns", ".", "OS", "PFS", "Var1", "Var2", "surv",
  "os_model", "parameter", "ann_text"
))

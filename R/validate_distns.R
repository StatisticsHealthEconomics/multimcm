
#' Validate distribution
#'
#' Partial match on start of word.
#' @keywords internal
#'
validate_distns <- function(model) {
  if (missing(model) || length(model) == 0) {
    stop("distribution not available.", call. = FALSE)
  }

  choices <- c(
    exp = "exp",
    exponential = "exp",
    weibull = "weibull",
    gompertz = "gompertz",
    loglogistic = "loglogistic",
    llogis = "loglogistic",
    llogistic = "loglogistic",
    lognormal = "lognormal",
    lnorm = "lognormal",
    gengamma = "gengamma",
    gen_gamma = "gengamma",
    gamma = "gengamma"
  )

  match_single <- function(x) {
    if (is.na(x) || !is.character(x) || nchar(trimws(x)) == 0) {
      stop("distribution not available.", call. = FALSE)
    }
    x_clean <- tolower(trimws(x))

    if (x_clean %in% names(choices)) {
      return(unname(choices[x_clean]))
    }

    matches <- choices[startsWith(names(choices), x_clean)]
    unique_matches <- unique(matches)

    if (length(unique_matches) == 1) {
      return(unname(unique_matches))
    }

    if (length(unique_matches) == 0) {
      rev_matches <- choices[startsWith(x_clean, names(choices))]
      unique_rev <- unique(rev_matches)
      if (length(unique_rev) == 1) {
        return(unname(unique_rev))
      }
    }

    if (length(unique_matches) > 1) {
      stop(sprintf("Ambiguous distribution '%s': matches multiple distributions (%s).",
                   x, paste(unique_matches, collapse = ", ")), call. = FALSE)
    }

    stop("distribution not available.", call. = FALSE)
  }

  vapply(model, match_single, character(1), USE.NAMES = FALSE)
}


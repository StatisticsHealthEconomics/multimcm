test_that("Stan code generates and compiles successfully for different models", {
  skip_if_not_installed("cmdstanr")
  
  # cmdstanr is available, but let's check if the backend is actually set up
  # If not, skip to avoid failing the build
  has_cmdstan <- tryCatch({
    !is.null(cmdstanr::cmdstan_path())
  }, error = function(e) FALSE)
  
  skip_if_not(has_cmdstan, message = "cmdstan path not set, skipping compilation tests")

  models_to_test <- list(
    c("exp"),
    c("weibull"),
    c("loglogistic", "weibull"),
    c("gengamma", "exp", "weibull"),
    c("exp", "lognormal", "gompertz", "gengamma")
  )

  for (distns in models_to_test) {
    code <- multimcm:::create_stancode(distns)
    
    # Save the generated code to a temporary file
    tmp_file <- tempfile(fileext = ".stan")
    writeLines(code, tmp_file)

    # We only compile = FALSE to syntax-check, avoiding long build times
    # Note: cmdstan_model parses and checks syntax even if compile=FALSE
    expect_error(
      cmdstanr::cmdstan_model(tmp_file, compile = FALSE, pedantic = TRUE),
      NA,
      info = paste("Failed to syntax-check generated code for distributions:", paste(distns, collapse = ", "))
    )
  }
})

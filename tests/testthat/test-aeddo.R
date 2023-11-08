test_that("Proper 'aedseo' class is returned", {
  # Generate some sample data
  y <- rnbinom(n = 40, size = 10, prob = 0.7)

  # Run the algorithm
  aeddo_results <- aeddo(
    y = y,
    n = 1,
    formula = y ~ 1,
    k = 36,
    sig_level = 0.95,
    exclude_past_outbreaks = TRUE,
    init_theta = c(1, 1),
    lower = c(-Inf, 1e-6),
    upper = c(Inf, 1e2),
    method = "L-BFGS-B"
  )

  # Test that the correct class is returned
  expect_s3_class(aeddo_results, "aedseo")
})

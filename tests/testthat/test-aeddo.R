test_that("Proper 'aedseo' class is returned", {
  # Sample some data
  tbl_data <- tibble::tibble(
    time = 1:100,
    y = rnbinom(
      n = 100,
      size = 10,
      prob = 0.7
    ),
    n = 1
  )

  # Run the algorithm
  aeddo_results <- aeddo(
    data = tbl_data,
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
  expect_s3_class(aeddo_results, "aeddo")
})

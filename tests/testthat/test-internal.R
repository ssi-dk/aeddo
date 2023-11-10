test_that("check_nll_poisson_gamma_inputs throws an error for invalid inputs", {
  # Invalid inputs for testing
  invalid_theta <- "invalid"
  invalid_data <- data.frame(x = c(1, 2, 3), n = c(7, 8, 9))
  invalid_data_class <- list(x = c(1, 2, 3), y = c(1, 2, 3), n = c(7, 8, 9))
  invalid_formula <- "invalid"
  valid_formula <- y ~ log(n)

  # Test invalid theta
  expect_error(
    check_nll_poisson_gamma_inputs(invalid_theta, invalid_data, valid_formula)
  )

  # Test missing 'y' in data
  expect_error(
    check_nll_poisson_gamma_inputs(1:3, invalid_data, valid_formula)
  )

  # Test invalid data class
  expect_error(
    check_nll_poisson_gamma_inputs(1:3, invalid_data_class, valid_formula)
  )

  # Test invalid formula
  expect_error(
    check_nll_poisson_gamma_inputs(
      1:3,
      data.frame(y = c(4, 5, 6), n = c(7, 8, 9)),
      invalid_formula
    )
  )
})

test_that(
  "check_nll_poisson_gamma_inputs does not throw an error for valid inputs",
  {
    # Valid inputs for testing
    valid_theta <- c(1, 2, 3)
    valid_data <- data.frame(y = c(4, 5, 6), n = c(7, 8, 9))
    valid_formula <- ~ y + n

    # Test valid inputs
    expect_no_error(
      check_nll_poisson_gamma_inputs(valid_theta, valid_data, valid_formula)
    )
  }
)

test_that("check_aeddo_inputs throws an error for invalid inputs", {
  # Invalid inputs for testing
  invalid_data <- data.frame(x = c(1, 2, 3), n = c(7, 8, 9))
  invalid_data_class <- list(x = c(1, 2, 3), y = c(1, 2, 3), n = c(7, 8, 9))
  invalid_formula <- "not_formula"
  invalid_k <- -5
  invalid_sig_level <- 1.5
  invalid_exclude_past_outbreaks <- "not_logical"
  invalid_init_theta <- "not_numeric"
  invalid_lower <- "not_numeric"
  invalid_upper <- "not_numeric"
  invalid_method <- "invalid_choice"
  valid_data <- data.frame(time = 1:3, y = c(4, 5, 6), n = c(7, 8, 9))
  valid_formula <- y ~ 1
  valid_k <- 2
  valid_sig_level <- 0.95
  valid_exclude_past_outbreaks <- TRUE
  valid_init_theta <- 1
  valid_lower <- 0
  valid_upper <- 100
  valid_method <- "BFGS"

  # Test missing 'y' in data
  expect_error(
    check_aeddo_inputs(
      invalid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid data class
  expect_error(
    check_aeddo_inputs(
      invalid_data_class,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid formula
  expect_error(
    check_aeddo_inputs(
      valid_data,
      invalid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid k
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      invalid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid sig_level
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      invalid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    ),
  )

  # Test invalid exclude_past_outbreaks
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      invalid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid init_theta
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      invalid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid lower
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      invalid_lower,
      valid_upper,
      valid_method
    )
  )

  # Test invalid upper
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      invalid_upper,
      valid_method
    )
  )

  # Test invalid method
  expect_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      invalid_method
    )
  )
})

test_that("check_aeddo_inputs does not throw an error for valid inputs", {
  # Valid inputs for testing
  valid_data <- data.frame(time = 1:3, y = c(4, 5, 6), n = c(7, 8, 9))
  valid_formula <- y ~ 1
  valid_k <- 2
  valid_sig_level <- 0.95
  valid_exclude_past_outbreaks <- TRUE
  valid_init_theta <- 1
  valid_lower <- 0
  valid_upper <- 100
  valid_method <- "BFGS"

  # Test valid inputs
  expect_no_error(
    check_aeddo_inputs(
      valid_data,
      valid_formula,
      valid_k,
      valid_sig_level,
      valid_exclude_past_outbreaks,
      valid_init_theta,
      valid_lower,
      valid_upper,
      valid_method
    )
  )
})

test_that("Does the negative log likelihood function work", {

  # Sample some data
  tbl_data <- tibble::tibble(
    y = rnbinom(
      n = 100,
      size = 10,
      prob = 0.7
    ),
    n = 1
  )

  # Construct vector with model parameters
  theta <- c(1, 1)

  # Assign a fixed effects model formula
  fixed_effects_formula <- y ~ 1

  # Calculate the negative log likelihood
  nll <- nll_poisson_gamma(
    theta = theta,
    data = tbl_data,
    formula = fixed_effects_formula
  )

  # Expect that results is numeric
  expect_true(object = is.numeric(nll))
})

poisson_gamma <- function(
    theta,
    y,
    n = 1,
    formula) {
  # Create a data.frame with the observation 'y' and population sizes 'n'
  observations_data <- data.frame(
    y = y,
    n = n
  )

  # Construct design matrix for the model
  design_matrix <- model.matrix(
    object = formula,
    data = observations_data
  )

  # Extract number of parameters in the fixed effects model
  n_parameters <- ncol(design_matrix)

  # Construct vector with fixed effects parameters
  beta <- theta[1:n_parameters]
  # ... and model parameters
  phi <- theta[-(1:n_parameters)]

  # Define the lambda parameter
  lambda <- exp(design_matrix %*% beta - log(n))

  # Construct "size" and "probability" for the negative binomial distribution
  size <- 1 / phi
  probability <- 1 / (lambda * phi + 1)

  # Calculate the log-likelihood
  log_likelihood <- sum(
    dnbinom(
      x = y,
      size = size,
      prob = probability,
      log = TRUE
    )
  )

  # Return the negative log likelihood
  negative_log_likelihood <- -log_likelihood

  return(negative_log_likelihood)
}

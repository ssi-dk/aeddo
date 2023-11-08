#' Negative Log-Likelihood for Poisson Gamma Model
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculate the negative log-likelihood for the Poisson Gamma modeling
#' framework.
#'
#' @param theta A numeric vector containing model parameters. The first part of
#' the vector represents fixed effects, and the remaining part represents model
#' parameters.
#' @param y A numeric vector of observed counts.
#' @param n A numeric vector of population sizes (default is 1).
#' @param formula A formula specifying the model structure.
#'
#' @return The negative log-likelihood value.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' nll_poisson_gamma(c(0.5, 0.1), c(5, 8, 6), n = 100, y ~ 1)
nll_poisson_gamma <- function(
    theta,
    data,
    formula) {

  # Extract the observations
  y <- data$y
  # ... and population size
  n <- data$n

  # Construct design matrix for the model
  design_matrix <- stats::model.matrix(
    object = formula,
    data = data
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
    stats::dnbinom(
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

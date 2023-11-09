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
#' @param data A tibble containing the time series data, including columns 'y'
#' for observed values,'n' for population size, and other covariates of
#' interest.
#' @param formula A formula specifying the model structure.
#'
#' @return The negative log-likelihood value.
#'
#' @export
#'
#' @examples
#' # Initial parameters
#' theta <- c(0.5, 0.1)
#'
#' # Sample data
#' data <- data.frame(
#'   y = c(10, 15, 20, 30, 50, 100, 200, 40, 20, 10),
#'   n = c(100, 150, 200, 300, 500, 1000, 2000, 400, 200, 100)
#' )
#'
#' # Fixed effects model formula
#' fixed_effects_formula <- y ~ 1
#'
#' # Calculate negative log likelihood
#' nll_poisson_gamma(
#'   theta = theta,
#'   data = data,
#'   formula = fixed_effects_formula
#' )
nll_poisson_gamma <- function(
    theta,
    data,
    formula) {

  # Initialize the assertion collections
  coll <- checkmate::makeAssertCollection()

  # Assert function inputs
  checkmate::assert_numeric(
    theta,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_class(
    data, classes = c("data.frame"),
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_names(
    colnames(data),
    must.include = c("y", "n"),
    add = coll
  )
  checkmate::assert_formula(
    formula,
    null.ok = FALSE,
    add = coll
  )

  # Report assertion collections
  checkmate::reportAssertions(coll)

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

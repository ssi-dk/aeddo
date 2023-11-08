#' Automated and Early Detection of Disease Outbreaks
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function performs automated an early detection of disease outbreaks,
#' (aeddo), on a time series data set. It utilizes hierarchical models in an
#' innovative manner to infer one-step ahead random effects. In turn, these
#' random effects are used directly to characterize an outbreak.
#'
#' @param data A tibble containing the time series data, including columns 'y'
#' for observed values,'n' for population size, and other covariates of
#' interest.
#' @param formula A model formula for the fixed effects in the  hierarchical
#' model to fit to the data.
#' @param k An integer specifying the rolling window size employed for parameter
#' estimation.
#' @param sig_level The quantile from the random effects distribution used for
#' defining the for outbreak detection threshold, a numeric value between 0 and
#' 1.
#' @param exclude_past_outbreaks logical value indicating whether past outbreak
#' related observations should be excluded from future parameter estimation.
#' @param init_theta Initial values for model parameters in optimization.
#' @param lower Lower bounds for optimization parameters.
#' @param upper Upper bounds for optimization parameters.
#' @param method The optimization method to use, either "BFGS"  (default) or
#' "L-BFGS-B".
#'
#' @return An 'aedseo' object containing:
#'   - 'window_data': A list of [tibble::tibble()], each representing a window of observations.
#'   - 'reference_data': A list of [tibble::tibble()], each representing a reference data point.
#'   - 'phi': The dispersion parameter.
#'   - 'lambda': The estimated outbreak intensity.
#'   - 'u': The one-step ahead random effect.
#'   - 'u_probability': The probability of observing the one-step ahead random effect.
#'   - 'outbreak_alarm': Logical. Indicates if an outbreak is detected.
#'
#' @export
#'
#' @examples
#' # Sample time series data
#' data <- data.frame(
#'   y = c(10, 15, 20, 30, 50, 100, 200, 40, 20, 10),
#'   n = c(100, 150, 200, 300, 500, 1000, 2000, 400, 200, 100)
#' )
#' # Define formula for modeling
#' formula <- y ~ 1
#' # Detect outbreaks
#' aeddo_results <- aeddo(
#'   data = data,
#'   formula = formula,
#'   k = 5,
#'   sig_level = 0.95,
#'   exclude_past_outbreaks = TRUE,
#'   init_theta = c(1,1),
#'   lower = c(-Inf, 1e-6),
#'   upper = c(Inf, 1e2),
#'   method = "L-BFGS-B")
#' # Print the results
#' print(aeddo_results)
aeddo <- function(
    data,
    formula = formula(),
    k,
    sig_level = 0.95,
    exclude_past_outbreaks = TRUE,
    init_theta = numeric(),
    lower,
    upper,
    method = "BFGS") {

  # Count the number of observations
  n_observation <- dplyr::count(data) %>%
    purrr::pluck("n")

  # Prepare an empty tibble for results with class 'aedseo'.
  results <- tibble::new_tibble(
    x = tibble::tibble(
      window_data = list(),
      reference_data = list(),
      phi = numeric(),
      lambda = numeric(),
      u = numeric(),
      u_probability = numeric(),
      outbreak_alarm = logical()
    ),
    class = "aedseo"
  )

  # Loop over the observations to perform windowed estimation
  for (i in 1:(n_observation - k)) {

    # Extract data point for this estimation window
    window_data <- data %>%
      dplyr::filter(dplyr::row_number() %in%  1:(i + k - 1))
    # ... and the reference data
    reference_data <- data %>%
      dplyr::filter(dplyr::row_number() == i + k)

    # Extract the observation at reference time point
    reference_y <- reference_data %>%
      purrr::pluck("y")
    # ... and the population size
    reference_n <- reference_data %>%
      purrr::pluck("n")

    # Construct a design matrix for the reference data
    reference_design_matrix <- stats::model.matrix(
      object = formula,
      data = reference_data
    )

    # Gather all arguments in a list for`do.call()`
    optimiser_arguments <- list(
      par = init_theta,
      data = window_data,
      formula = formula,
      fn = nll_poisson_gamma,
      method = method
    )

    if (method == "L-BFGS-B") {
      optimiser_arguments$lower <- lower
      optimiser_arguments$upper <- upper
    }

    # Obtain parameters with maximum likelihood estimation
    optimized_param <- do.call(what = "optim", args = optimiser_arguments)

    # Extract fixed effects parameters
    fixef_parameter <- optimized_param$par[seq_len(
      ncol(reference_design_matrix)
    )]
    # ... and dispersion parameter, phi
    phi <- optimized_param$par[length(optimized_param$par)]

    # Establish one-step ahead lambda using reference observation
    lambda <- c(
      exp(reference_design_matrix %*% fixef_parameter - log(reference_n))
      )

    # Infer the one-step ahead random effect
    u <- (reference_y * phi + 1) / (lambda * phi + 1)

    # Compute probability this random effects is observed
    u_probability <- stats::pgamma(
      q = u,
      shape = 1 / phi,
      scale = phi
    )

    # Does the inferred one-step ahead random effect exceed significance level?
    outbreak_alarm <- u_probability >= sig_level

    # Gather results for return
    results <- results %>%
      tibble::add_row(
        window_data = list(window_data),
        reference_data = list(reference_data),
        phi = phi,
        lambda = lambda,
        u = u,
        u_probability = u_probability,
        outbreak_alarm = outbreak_alarm
      )
  }

  return(results)
}

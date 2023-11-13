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
#' @return A [tibble][tibble::tibble-package]-like 'aedseo' object containing:
#'   - 'window_data': A list of [tibble][tibble::tibble-package], each
#'   representing the data for this windowed parameter estimation.
#'   - 'reference_data': A list of [tibble][tibble::tibble-package], each
#'   representing the data for the reference time point.
#'   - 'phi': The dispersion parameter.
#'   - 'lambda': The estimated outbreak intensity.
#'   - 'u': The one-step ahead random effect.
#'   - 'u_probability': The probability of observing the one-step ahead
#'   random effect.
#'   - 'outbreak_alarm': Logical. Indicates if an outbreak is detected.
#'
#' @export
#'
#' @examples
#' # Create an example aedseo_tsd object
#' aeddo_data <- data.frame(
#'   time = as.Date(c(
#'     "2023-01-01",
#'     "2023-01-02",
#'     "2023-01-03",
#'     "2023-01-04",
#'     "2023-01-05",
#'     "2023-01-06"
#'   )),
#'   y = c(100, 120, 180, 110, 130, 140),
#'   n = 1
#' )
#'
#' # Supply a model formula
#' fixed_effects_formula <- y ~ 1
#'
#' # Choose a size for the rolling window
#' k = 2
#' # ... and quantile for the threshold
#' sig_level = 0.9
#'
#' # Employ the algorithm
#' aeddo_results <- aeddo(
#'   data = aeddo_data,
#'   formula = fixed_effects_formula,
#'   k = k,
#'   sig_level = sig_level,
#'   exclude_past_outbreaks = TRUE,
#'   init_theta = c(1, 0),
#'   lower = c(-Inf, 1e-6),
#'   upper = c(Inf, 1e2),
#'   method = "L-BFGS-B"
#'   )
#' # Print the results
#' print(aeddo_results)
aeddo <- function(
    data = data.frame(),
    formula = formula(),
    k = integer(),
    sig_level = 0.95,
    exclude_past_outbreaks = TRUE,
    init_theta = numeric(),
    lower = numeric(),
    upper = numeric(),
    method = "BFGS") {
  # Assert function inputs
  check_aeddo_inputs(
    data,
    formula,
    k,
    sig_level,
    exclude_past_outbreaks,
    init_theta,
    lower,
    upper,
    method
  )

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
    class = "aeddo",
    data = data,
    k = k
  )

  # Initilize past_outbreaks if we want to omit outbreak related observations
  if (exclude_past_outbreaks == TRUE) {
    past_outbreaks <- tibble::as_tibble(
      lapply(data, function(col) col[0])
    )
  }

  # Loop over the observations to perform windowed estimation
  for (i in 1:(n_observation - k)) {
    # Extract data point for this estimation window
    window_data <- data %>%
      dplyr::filter(dplyr::row_number() %in% i:(i + k - 1))
    # ... and the reference data
    reference_data <- data %>%
      dplyr::filter(dplyr::row_number() == i + k)

    # Exclude past observations, if they were deemed an outbreak
    # Turned on by 'excludePastOutbreaks = TRUE'
    if (exclude_past_outbreaks == TRUE) {
      if (nrow(past_outbreaks) > 0) {
        window_data <- window_data %>%
          dplyr::setdiff(past_outbreaks)
      }
    }

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

    if (exclude_past_outbreaks == TRUE && outbreak_alarm == TRUE) {
      past_outbreaks <- past_outbreaks %>%
        dplyr::bind_rows(reference_data)
    }
  }

  return(results)
}

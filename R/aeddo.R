#' Automated and Early Detection of Disease Outbreaks
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function performs automated an early detection of disease outbreaks
#' (aeddo) on a time series data set. It utilizes hierarchical models in an
#' innovative manner to infer one-step ahead random effects. In turn, these
#' random effects are used directly to characterize an outbreak.
#'
#' @param data
#' @param formula
#' @param k
#' @param sig_level
#' @param exclude_past_outbreaks
#' @param init_theta
#' @param lower
#' @param upper
#' @param method
#'
#' @return
#' @export
#'
#' @examples
aeddo <- function(
    data,
    formula,
    k,
    sig_level,
    exclude_past_outbreaks,
    init_theta,
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

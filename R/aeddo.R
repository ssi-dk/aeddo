aeddo <- function(
    y,
    n = 1,
    formula,
    k,
    sig_level,
    exclude_past_outbreaks,
    init_theta,
    lower,
    upper,
    method = "BFGS") {
  # Count the number of observations
  n_observations <- length(y)

  # Prepare an empty tibble for results with class 'aedseo'.
  results <- tibble::new_tibble(
    x = tibble::tibble(
      y = integer(),
      n = integer(),
      phi = numeric(),
      lambda = numeric(),
      u = numeric(),
      u_probability = numeric(),
      outbreak_alarm = logical()
    ),
    class = "aedseo"
  )

  # Loop over the observations to perform windowed estimation
  for (i in 1:(n_observations - k)) {
    # Extract the observations for this window
    window_observation <- y[i:(i + k - 1)]
    # ... and the reference observation
    reference_observation <- y[i + k]

    # Create data.frame with
    reference_data <- data.frame(
      y = reference_observation,
      n = n
    )
    # Construct a design matrix for the reference data
    reference_design_matrix <- stats::model.matrix(
      object = formula,
      data = reference_data
    )

    # Gather all arguments in a list for`do.call()`
    optimiser_arguments <- list(
      par = init_theta,
      y = window_observation,
      n = n,
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
    lambda <- c(exp(reference_design_matrix %*% fixef_parameter - log(n)))

    # Infer the one-step ahead random effect
    u <- (reference_observation * phi + 1) / (lambda * phi + 1)

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
        y = reference_observation,
        n = n,
        phi = phi,
        lambda = lambda,
        u = u,
        u_probability = u_probability,
        outbreak_alarm = outbreak_alarm
      )
  }

  return(results)
}

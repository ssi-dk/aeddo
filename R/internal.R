##' Check and assert the inputs for the aeddo function.
#'
#' This function is for internal use and checks the validity of inputs to the
#' aeddo function. It performs various checks using the checkmate package to
#' ensure that the inputs conform to expected types and values.
#'
#' @param data A data frame containing the time series data.
#' @param formula A model formula for the fixed effects in the hierarchical
#' model.
#' @param k An integer specifying the rolling window size employed for
#' parameter estimation.
#' @param sig_level The quantile from the random effects distribution used for
#' defining the outbreak detection threshold.
#' @param exclude_past_outbreaks Logical indicating whether past
#' outbreak-related observations should be excluded from future parameter
#' estimation.
#' @param init_theta Initial values for model parameters in optimization.
#' @param lower Lower bounds for optimization parameters.
#' @param upper Upper bounds for optimization parameters.
#' @param method The optimization method to use, either "BFGS" or "L-BFGS-B".
#'
#' @return Returns nothing. Raises errors if the inputs do not meet the
#' specified criteria.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # This function is for internal use and is called within the aeddo function.
#' # It is not intended to be called directly by users.
#' }
check_aeddo_inputs <- function(
    data,
    formula,
    k,
    sig_level,
    exclude_past_outbreaks,
    init_theta,
    lower,
    upper,
    method) {
  coll <- checkmate::makeAssertCollection()

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
  checkmate::assert_count(
    k,
    na.ok = FALSE,
    positive = TRUE,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_number(
    sig_level,
    na.ok = FALSE,
    lower = 0,
    upper = 1,
    finite = TRUE,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_flag(
    exclude_past_outbreaks,
    na.ok = FALSE,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_numeric(
    lower,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_numeric(
    upper,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_numeric(
    init_theta,
    null.ok = FALSE,
    add = coll
  )
  checkmate::assert_choice(
    method,
    choices = c("BFGS", "L-BFGS-B"),
    add = coll
  )

  # Report assertions
  checkmate::reportAssertions(coll)
}

#' Check and assert the inputs for the nll_poisson_gamma function.
#'
#' This function is for internal use and checks the validity of inputs to the
#' nll_poisson_gamma function. It performs various checks using the checkmate
#' package to ensure that the inputs conform to expected types and values.
#'
#' @param theta Numeric vector of parameters.
#' @param data A data frame containing the time series data.
#' @param formula A model formula for the fixed effects in the hierarchical
#' model.
#'
#' @return Returns nothing. Raises errors if the inputs do not meet the
#' specified criteria.
#'
#' @keywords internal
#'
#' @examples
#'
#' \dontrun{
#' # This function is for internal use and is called within the
#' # nll_poisson_gamma function. It is not intended to be called directly by
#' # users.
#' }
check_nll_poisson_gamma_inputs <- function(
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

  # Report assertions
  checkmate::reportAssertions(coll)
}

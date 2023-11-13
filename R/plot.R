#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'  This function generates a complete 'ggplot' object suitable for
#'  visualizing time series data in an `aeddo` object. It creates a line
#'  plot connecting the observations and adds points at each data point.
#'
#' @param x An `aeddo` object
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the time series data.
#'
#' @aliases plot
#'
#' @seealso [autoplot()]
#'
#' @examples
#' # Create an example aeddo object
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
#' k <- 2
#' # ... and quantile for the threshold
#' sig_level <- 0.9
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
#' )
#'
#' # Create a ggplot visualization for the aeddo object
#' plot(aeddo_results)
#' @importFrom graphics plot
#' @rdname plot
#' @method plot aeddo
#' @export
plot.aeddo <- function(x, ...) {
  suppressWarnings(print(autoplot(x, ...)))
}

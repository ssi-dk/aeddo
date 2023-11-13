#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'  This function generates a complete 'ggplot' object suitable for
#'  visualizing time series data in an `aeddo` object. It creates a line
#'  plot connecting the observations and adds points at each data point.
#'
#' @param object An `aeddo` object
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the time series data.
#'
#' @aliases autoplot
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
#' autoplot(aeddo_results)
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#' @rdname autoplot
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}
#' @rdname autoplot
#' @method autoplot aeddo
#' @export
autoplot.aeddo <- function(object, ...) {
  # Extract the supplied data
  data <- attr(object, "data")
  # ... and the window width
  k <- attr(object, "k")

  # Unnest the results
  unnested_object_results <- object %>%
    tidyr::unnest("reference_data")

  # Join with original data, to visualize full series
  joined_results <- data %>%
    dplyr::full_join(
      y = unnested_object_results,
      by = dplyr::join_by("time", "y", "n")
    )

  # Extract the observations used for training
  training_dates <- utils::head(data$time, k)

  # Make a nice 'ggplot' visualization
  suppressWarnings(
    joined_results %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = .data$time,
          y = .data$y,
          alpha = .data$outbreak_alarm
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::annotate(
        geom = "rect",
        xmin = training_dates[1] - Inf,
        xmax = training_dates[k],
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.2
      ) +
      ggplot2::guides(alpha = "none")
  )
}

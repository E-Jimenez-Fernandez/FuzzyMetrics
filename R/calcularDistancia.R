#' Compute initial fuzzy metric
#'
#' This function calculates an initial fuzzy metric by transforming a normalized data frame into a fuzzy distance matrix.
#' The transformation uses the formula `f_y = 1 / (1 + X)`, where `X` is the normalized data.
#'
#' @param x A normalized data frame where rows represent observations and columns represent indicators.
#'
#' @return A matrix with fuzzy distances for each indicator, where values are between 0 and 1.
#' @export
#'
#' @examples
#' # Example 1: Compute fuzzy distances for a simple normalized data frame
#' normalized_data <- data.frame(
#'   indicator1 = c(0.1, 0.3, 0.5),
#'   indicator2 = c(0.6, 0.8, 0.2),
#'   indicator3 = c(0.7, 0.9, 0.4)
#' )
#' calcularDistancia(normalized_data)
#'
#' # Example 2: Apply on data normalized with polarity
#' custom_data <- data.frame(
#'   GDP = c(0.2, 0.5, 0.8),
#'   Unemployment = c(0.9, 0.7, 0.3),
#'   Literacy = c(0.1, 0.6, 0.9)
#' )
#' # Compute fuzzy distances after normalization
#' fuzzy_distances <- calcularDistancia(custom_data)
#' print(fuzzy_distances)

calcularDistancia <- function(x) {
  # Convert data.frame to matrix for calculations
  X <- as.matrix(x)

  # Calculate the fuzzy distances using the formula 1 / (1 + X)
  f_y <- 1 / (1 + X)

  # Keep the original column names
  colnames(f_y) <- colnames(X)

  # Convert to matrix and return
  mI <- as.matrix(f_y)
  return(mI)
}


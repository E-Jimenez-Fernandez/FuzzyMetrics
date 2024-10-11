#' Compute composite indicator
#'
#' This function calculates a composite indicator by multiplying similarity measures across all indicators for each observation.
#' It computes the product of the values across columns for each row in the input matrix.
#'
#' @param x A matrix of similarity measures, where rows represent observations and columns represent indicators.
#'
#' @return A numeric vector representing the composite indicator for each observation.
#' @export
#'
#' @examples
#' # Example 1: Compute composite indicator from similarity matrix
#' similarity_matrix <- matrix(
#'   c(0.9, 0.8, 0.7,
#'     0.6, 0.5, 0.7,
#'     0.4, 0.9, 0.6),
#'   nrow = 3, byrow = TRUE
#' )
#' # Compute the composite indicator
#' CompInd(similarity_matrix)
#'
#' # Example 2: Composite indicator with different similarity measures
#' similarity_matrix2 <- matrix(
#'   c(0.95, 0.85, 0.75,
#'     0.65, 0.55, 0.85,
#'     0.45, 0.75, 0.65),
#'   nrow = 3, byrow = TRUE
#' )
#' # Apply the function and calculate the composite indicator
#' composite <- CompInd(similarity_matrix2)
#' print(composite)
#'

CompInd <- function(x) {
  # n is the number of rows (observations) and m is the number of columns (indicators).
  n <- nrow(x)
  m <- ncol(x)

  # Initialise the DFS vector with 1s in order to calculate the accumulated product.
  DFS <- rep(1, n)  # Vector en lugar de matriz para simplificar el cálculo

  # Multiply the values in each column by the rows, accumulating the product.
  for (j in 1:m) {
    DFS <- DFS * x[, j]
  }

  # Returns the vector with the final values of the composite indicator.
  return(DFS)
}

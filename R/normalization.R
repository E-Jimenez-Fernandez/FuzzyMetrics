#' Normalize the data with respect to indicator polarity
#'
#' This function normalizes the columns of a data frame based on the polarity of the indicators.
#' Positive polarity (e.g., variables where higher values are better) is normalized using the `norm_maxmin` method,
#' while negative polarity (e.g., variables where lower values are better) is normalized using `norm_minmax`.
#'
#' @param x A data frame of input values where rows are observations and columns are indicators.
#' @param polarity A vector of column indices indicating the positive polarity of indicators (optional).
#'        If not provided, all indicators will be treated as having negative polarity.
#'
#' @return A data frame where the values have been normalized between 0 and 1.
#' @export
#'
#' @examples
#' # Example 1: All indicators with negative polarity
#' data <- data.frame(
#'   indicator1 = c(10, 20, 30),
#'   indicator2 = c(5, 25, 35),
#'   indicator3 = c(50, 10, 0)
#' )
#' normalization(data)
#'
#' # Example 2: Positive and negative polarity
#' # Suppose indicator1 and indicator3 have positive polarity (i.e., higher is better),
#' # and indicator2 has negative polarity (i.e., lower is better).
#' polarity_vector <- c(1, 3)
#' normalization(data, polarity = polarity_vector)
#'
#' # Example 3: Custom data with both positive and negative polarities
#' custom_data <- data.frame(
#'   GDP = c(50000, 60000, 70000),
#'   Unemployment = c(10, 8, 6),
#'   Literacy = c(0.9, 0.85, 0.95)
#' )
#' # Assume GDP and Literacy are positive polarity indicators, Unemployment is negative
#' normalization(custom_data, polarity = c(1, 3))
normalization <- function(x, polarity=NULL) {
  if (!is.data.frame(x)) {
    stop("Error: 'x' debe ser un data.frame.")
  }

  #
  names_var <- colnames(x)
  names_regions <- rownames(x)

  # Number f columns
  m <- dim(x)[2]
  columns <- c(1:m)

  # Asign columns with positive polarity
  pospol <- polarity
  if(!is.null(polarity)){
    negpol <- columns[-pospol]
  } else {
    negpol <- columns
  }

  #
  normdata <- matrix(0, ncol = ncol(x), nrow = nrow(x))

  # Function to normalise in range 0-1, if the higher value is better
  norm_minmax <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }

  # Function to normalise in range 0-1, if lower value is better
  norm_maxmin <- function(x) {
    (max(x) - x) / (max(x) - min(x))
  }

  # Normalize columns with + polarity
  for (j in pospol) {
    normdata[, j] <- norm_maxmin(x[, j])
  }

  # Normalize columns with - polarity
  for (j in negpol) {
    normdata[, j] <- norm_minmax(x[, j])
  }

  normdata <- as.data.frame(normdata)

  # Restore colnames and rownames
  colnames(normdata) <- names_var
  rownames(normdata) <- names_regions

  return(normdata)
}

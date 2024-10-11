#' Compute weights through MARS and variable importance
#'
#' This function calculates the weights of variables using the MARS (Multivariate Adaptive Regression Splines) model and evaluates variable importance based on the model's residual sum of squares (RSS).
#' The function is useful for determining the relative importance of different variables in explaining a composite indicator.
#'
#' @param x A normalized data frame where rows represent observations and columns represent indicators.
#' @param Compind A numeric vector representing the composite indicators for the observations.
#' @param degrees An integer specifying the degree of interactions to consider in the MARS model.
#' @param nfold Number of folds for cross-validation (default is 5).
#' @param trace Amount of information displayed during model fitting (default is 0.5).
#'
#' @importFrom dplyr mutate
#' @importFrom earth earth
#' @importFrom earth evimp
#' @importFrom dplyr %>%
#'
#' @return A list containing:
#'   \item{res.ord}{A numeric vector representing the weights for each variable.}
#'   \item{colnames(dat_os)}{A character vector with the names of the variables.}
#' @export
#'
#' @examples
#' # Generate a dataset with more rows (observations) and columns (variables)
#' set.seed(123)
#' normalized_data <- data.frame(
#'   GDP = runif(100, 0, 1),
#'   Unemployment = runif(100, 0, 1),
#'   Literacy = runif(100, 0, 1),
#'   Education = runif(100, 0, 1),
#'   Healthcare = runif(100, 0, 1),
#'   Inflation = runif(100, 0, 1),
#'   Exports = runif(100, 0, 1),
#'   Imports = runif(100, 0, 1)
#' )
#'
#' # Create a vector of composite indicators (e.g. the average of the above variables)
#' comp_ind <- rowMeans(normalized_data)
#'
#' # Specify the degree of interactions for the MARS model
#' degrees <- 2  # Allow up to second-degree interactions between variables
#'
#' # Execute the function calculoFactoresPonderacion with the generated data
#' result <- calculoFactoresPonderacion(normalized_data, comp_ind, degrees)
#'
#' # Print the weights of the variables
#' print(result[[1]])  # Calculated weights for each variable
#' print(result[[2]])  # Names of the variables


# Definir la función 'calculoFactoresPonderacion'
calculoFactoresPonderacion <- function(x, Compind, degrees, nfold = 5, trace = .5) {
  # Number of variables in the data frame
  m <- ncol(x)

  # Save the original variable names
  dat_os <- x

  # Add the composite indicator vector to the data frame as a new column
  x <- x %>% mutate(Cind = as.numeric(Compind))

  # Fit the MARS model with Cind as the dependent variable and the predictors in x
  mymodel <- earth(Cind ~ ., data = x, keepxy = TRUE, degree = degrees, nfold = nfold, trace = trace)

  # Evaluate variable importance using the evimp function
  p2 <- evimp(mymodel, trim = FALSE)

  # Check if evimp returned a valid result
  if (is.null(p2) || ncol(p2) < 1) {
    stop("Error: evimp() did not return valid results or the RSS column is missing.")
  }

  # Check if the "rss" column exists in p2
  if (!"rss" %in% colnames(p2)) {
    stop("Error: The RSS column is not present in the evimp output.")
  }

  # Get the variable names
  Variable <- row.names(p2)

  # Extract weights based on RSS (residual sum of squares)
  weights <- as.numeric(p2[,"rss"])

  # Adjust weights to avoid zero values and normalize them
  mini <- min(weights[weights > 0])
  Variable <- sub("-unused", "", Variable)  # Remove "-unused" from variable names
  weights[weights == 0] <- mini / m  # Avoid zero weights

  # Normalize weights to sum to 1 (or divide by 100 to get percentages)
  weights <- weights / 100

  # Convert the weights into a data frame and set the column names
  res <- data.frame(t(weights))
  colnames(res) <- Variable

  # Order the weights according to the original column names
  res.ord <- as.numeric(res[colnames(dat_os)])

  # Return a list containing the ordered weights and the variable names
  important <- list(res.ord, colnames(dat_os))

  return(important)
}

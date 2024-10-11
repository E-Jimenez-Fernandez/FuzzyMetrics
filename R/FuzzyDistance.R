#' Compute fuzzy distance with given factors
#'
#' This function calculates the fuzzy distance for a normalized data frame based on a set of given factors (weights).
#' It also computes a symmetric welfare matrix, which adjusts the normalized values based on the weights.
#'
#' @param x A normalized data frame where rows represent observations and columns represent indicators.
#' @param Factores A vector of weights, where each weight corresponds to the relative importance of an indicator.
#'
#'
#' @return A list containing:
#'   \item{DFS}{A numeric vector representing the fuzzy distance for each observation.}
#'   \item{Sym_welfare}{A matrix representing the symmetric welfare values based on the input factors and the normalized data.}
#' @export
#'
#' @examples
#' # Example 1: Compute fuzzy distance for normalized data with equal weights
#' normalized_data <- data.frame(
#'   indicator1 = c(0.1, 0.3, 0.5),
#'   indicator2 = c(0.6, 0.8, 0.2),
#'   indicator3 = c(0.7, 0.9, 0.4)
#' )
#' # Assume equal importance for all indicators
#' weights <- c(1, 1, 1)
#' FuzzyDistance(normalized_data, weights)
#'
#' # Example 2: Compute fuzzy distance with different weights
#' custom_data <- data.frame(
#'   GDP = c(0.2, 0.5, 0.8),
#'   Unemployment = c(0.9, 0.7, 0.3),
#'   Literacy = c(0.1, 0.6, 0.9)
#' )
#' # Different weights for each indicator
#' weights_custom <- c(0.5, 2, 1.5)
#' result <- FuzzyDistance(custom_data, weights_custom)
#' print(result$DFS)  # Fuzzy distance vector
#' print(result$Sym_welfare)  # Symmetric welfare matrix
#'

FuzzyDistance <- function(x, Factores) {
  # Obtener las dimensiones del data frame
  n <- dim(x)[1]  # Número de filas (observaciones)
  m <- dim(x)[2]  # Número de columnas (indicadores)

  # Crear una matriz para almacenar la matriz de bienestar simétrico
  Sym_welfare <- matrix(0, nrow = n, ncol = m)

  # Calcular la matriz de bienestar simétrico usando los factores (pesos)
  for (j in 1:m) {
    for (i in 1:n) {
      Sym_welfare[i, j] <- Factores[j] / (Factores[j] + x[i, j])
    }
  }

  # Calcular la distancia difusa (fuzzy distance) usando la función CompInd
  DFS <- CompInd(Sym_welfare)  # Este es un vector

  # Verificar si DFS es un vector o una matriz/data frame antes de asignar nombres de columna
  if (is.matrix(DFS) || is.data.frame(DFS)) {
    colnames(DFS) <- paste("fuzzydistance", sep = ".")
  }

  # Crear una lista con el vector de distancias difusas y la matriz de bienestar simétrico
  DFSY <- list(DFS = DFS, Sym_welfare = Sym_welfare)

  # Devolver la lista
  return(DFSY)
}


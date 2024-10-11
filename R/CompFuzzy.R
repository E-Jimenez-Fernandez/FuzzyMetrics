#' Main function to compute fuzzy composite indicators
#'
#' This function computes fuzzy composite indicators iteratively using normalization,
#' correlation analysis, fuzzy distance calculation, and MARS (Multivariate Adaptive Regression Splines) for weight estimation.
#' The process iterates until the error between successive iterations is below the specified tolerance or the maximum number of iterations is reached.
#'
#' @param x A data frame of input values where rows represent observations and columns represent indicators.
#' @param polarity A vector indicating the polarity of indicators (positive or negative).
#' @param err Error tolerance for stopping the iterative process.
#' @param iterations Maximum number of iterations allowed.
#' @param degrees Degree of interaction for the MARS model.
#' @param nfold Number of folds for cross-validation (default is 5).
#' @param trace Amount of information displayed during MARS model fitting (default is 0.5).
#'
#' @importFrom dplyr mutate
#' @importFrom earth earth
#' @importFrom earth evimp
#' @importFrom corrgram corrgram
#' @importFrom corrgram panel.shade panel.pie panel.txt
#'
#'
#' @return A list containing:
#'   \item{Fuzzydistance}{Final fuzzy distance values.}
#'   \item{Fuzzydistance_1}{Fuzzy distance from the previous iteration.}
#'   \item{Correlation}{Correlation matrix plot for the normalized data.}
#'   \item{iteration}{The number of iterations executed.}
#'   \item{Names.Single.Ind}{Names of the single indicators used in the model.}
#'   \item{Single.Ind}{Weights of the indicators derived from the MARS model.}
#'   \item{error}{Error values from each iteration.}
#'   \item{sym}{Symmetric welfare matrix from the final fuzzy distance computation.}
#' @export
#'
#' @examples
#' # Example 1: Compute fuzzy composite indicators with randomly generated data
#' set.seed(123)  # For reproducibility
#' large_data <- as.data.frame(matrix(runif(1000), nrow = 100, ncol = 10))  # 100 rows, 10 columns
#' polarity <- c(1, 3, 5, 7, 9)  # Assume some indicators have positive polarity
#' err_tolerance <- 0.001  # Error tolerance for stopping criteria
#' max_iterations <- 10  # Maximum iterations
#' degree_of_interaction <- 1  # Degree of interaction for MARS model
#' result <- CompFuzzy(large_data, polarity, err_tolerance, max_iterations, degree_of_interaction)
#' print(result$Fuzzydistance)  # Final fuzzy distance
#' print(result$iteration)  # Number of iterations executed
#'
#' # Example 2: Compute fuzzy composite indicators with more variables and data
#' set.seed(456)  # For reproducibility
#' big_data <- as.data.frame(matrix(runif(5000), nrow = 500, ncol = 10))  # 500 rows, 10 columns
#' polarity_large <- c(2, 4, 6, 8, 10)  # Different polarity assumptions
#' result_large <- CompFuzzy(big_data, polarity_large, err_tolerance, max_iterations, degree_of_interaction)
#' print(result_large$Single.Ind)  # Weights from the MARS model
#' print(result_large$error)  # Error values over iterations
#'


CompFuzzy <- function(x, polarity, err, iterations, degrees, nfold = 5, trace = .5) {
  if (!is.data.frame(x)) {
    stop("Error: 'x' debe ser un data.frame.")
  }

  n <- dim(x)[2]  # Número de indicadores
  resultados <- list()  # Lista para almacenar los resultados
  error.d <- numeric()  # Vector para almacenar los errores por iteración
  Weights <- matrix(nrow = 0, ncol = ncol(x))  # Matriz para almacenar los pesos

  # Normalización de los datos
  normdata <- normalization(x, polarity)

  # Generación de un gráfico de correlación (opcional, asume uso de corrgram)
  corrgr <- corrgram(normdata, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, main = "Correlation between each feature")

  # Calcular las distancias difusas iniciales
  mDif <- calcularDistancia(normdata)
  dps <- CompInd(mDif)  # Este ya es un vector

  iteracion <- 1  # Inicializar el contador de iteraciones

  # Bucle de iteración hasta que el error sea menor que el umbral o se alcancen las iteraciones máximas
  while ((error.d[iteracion] >= err || is.na(error.d[iteracion])) && (iteracion <= iterations)) {
    print(paste("Iteration", iteracion))

    # Calcular los factores de ponderación utilizando MARS
    mFac <- calculoFactoresPonderacion(normdata, dps, degrees)
    Weights <- rbind(Weights, mFac[[1]])  # Almacenar los pesos

    # Calcular la distancia difusa usando los factores ponderados
    ComposINd <- FuzzyDistance(normdata, mFac[[1]])
    df_ite <- ComposINd[[1]]  # Vector con la distancia difusa actual

    # Calcular el error como la diferencia entre las iteraciones
    error <- sum((dps - df_ite)^2)
    error.d[iteracion] <- error  # Almacenar el error

    # Condición de parada
    if (error < err || iteracion >= iterations) {
      break
    }

    iteracion <- iteracion + 1
    dps <- df_ite  # Actualizar el vector dps para la siguiente iteración
  }

  # Almacenar los resultados en la lista
  resultados$Fuzzydistance <- df_ite
  resultados$Fuzzydistance_1 <- dps
  resultados$Correlation <- corrgr
  resultados$iteration <- iteracion
  resultados$Names.Single.Ind <- mFac[[2]]
  resultados$Single.Ind <- mFac[[1]]
  resultados$error <- error.d
  resultados$sym <- ComposINd[[2]]

  # Devolver la lista de resultados
  return(resultados)
}

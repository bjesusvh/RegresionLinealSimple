#' Predicciones de un modelo de regresión.
#'
#' Realiza las predicciones de un modelo de regresión lineal simple.
#'
#' @param modelo_entrenado salida de la función \code{regresion}.
#' @param x_nuevo (vector) datos de la variable explicativa para los cuales se requiere la predicción.
#' @return Un vector de valores predichos.
#' @export
#'
#' @examples
#' \dontrun{
#' # Cargando la librería
#' library(RegLinS)
#'
#' # Entreno mi modelo
#' x <- c(1,2,3,4,5)
#' y <- c(5, 5.4, 6, 7, 9)
#' salida <- regresion(y = y, x = x)
#'
#' # Realizo predicciones
#' xnew <- c(2.7, 4.5, 3.1)
#' predictML(x_nuevo = xnew, modelo_entrenado = salida)
#' }
predictML <- function(x_nuevo, modelo_entrenado){

  # x_nuevo tienen que ser un vector

  # Extraigo el valor de beta 0 y beta 1
  beta0 <- modelo_entrenado$Coef[1,1]
  beta1 <- modelo_entrenado$Coef[2,1]

  # Realizamos las predicciones
  ypredichos <- beta0 + beta1*x_nuevo

  # Devuelve
  return(ypredichos)

}

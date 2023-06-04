#' Regresión lineal simple
#'
#' Realiza una regresión lineal simple.
#'
#' @param y (vector) datos de la respuesta.
#' @param x (vector) datos de la variable explicativa.
#' @return Una lista con los coeficientes de regresión, errores estándar, intervalo de confianza, y coeficiente de determinación.
#' @export
#'
#' @examples
#' \dontrun{
#' # Directorio de trabajo
#' ruta <- "incluir tu ruta aquí y tu archivo"
#'
#' # -----------------------------------
#' # Ejemplo 1:
#' MisDatos <- read.csv(ruta)
#' # Cargo la librería
#' library(RegLinS)
#' regresion(y = MisDatos$peso, x = MisDatos$ancho_corazon)
#'
#' # -----------------------------------
#' # Ejemplo 2:
#' x <- c(1,2,3,4,5, 6)
#' y <- c(5, 5.4, 6, 7, 9, 8)
#'
#' regresion(y = y, x = x)
#' }
regresion <- function(y, x){

  # cálculos preliminares
  n <- length(y)
  xbarra <- mean(x)
  ybarra <- mean(y)
  S_xy <- sum(y*(x-xbarra))
  S_xx <- sum((x - xbarra)^2)

  # Cálculo de beta 1 sombrero
  beta1 <- S_xy / S_xx

  # Cálculo de beta 0 sombrero
  beta0 <- ybarra - beta1*xbarra

  # Cálculo de y sombrero
  yajustado <- beta0 + beta1*x

  # Cálculo de la estimación de la varianza
  sigma2 <- sum((y - yajustado)^2)/(n-2)

  # Cálculo del error estándar para beta0
  ES_beta1 <- sqrt(sigma2 / S_xx)

  # Cálculo del error estándar para beta1
  ES_beta0 <- sqrt(sigma2 * (1/n + xbarra^2/S_xx))

  # Cálculo un intervalo de confianza para beta1
  ICi <- beta1 - 1.96*ES_beta1
  ICs <- beta1 + 1.96*ES_beta1

  # Cálculo del coeficiente de determinación o R^2
  TSS <- sum((y-ybarra)^2)
  R2 <- 1-(sqrt(sigma2)/sqrt(TSS))

  # Crear una una lista
  betas <- c(beta0, beta1)
  ES <- c(ES_beta0, ES_beta1)
  IC <- c(ICi, ICs)
  df <- data.frame(betas, ES)
  colnames(df) <- c("Betas", "Error_Std")

  resultado <- list("Coef" = df,
                    "Int_Conf_Beta1" = IC,
                    "R2" = R2)

  return(resultado)

}

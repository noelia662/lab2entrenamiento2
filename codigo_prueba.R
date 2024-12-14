# Función para leer los números desde el archivo
leer_numeros <- function(nombre_archivo) {
  # Verificar si el archivo existe
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe.")
  }
  # Leer el archivo y convertirlo a un vector de enteros
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

# Función para calcular los estadísticos y manejar los valores atípicos
calcular_estadisticos <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  # Verificar alta variabilidad
  if (desviacion_estandar > 10) {
    mensaje_variabilidad <- "Alta variabilidad en los datos."
  } else {
    mensaje_variabilidad <- "Variabilidad dentro de los límites normales."
  }
  
  # Devolver los resultados
  return(list(media = media, mediana = mediana, desviacion_estandar = desviacion_estandar, mensaje = mensaje_variabilidad))
}

# Función para calcular los cuadrados de los números
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# Función para guardar los resultados en un archivo
guardar_resultados <- function(nombre_archivo, estadisticos, cuadrados) {
  # Abrir el archivo para escribir
  cat("Estadísticos calculados:\n", file = nombre_archivo)
  cat(paste("Media: ", estadisticos$media, "\n"), file = nombre_archivo, append = TRUE)
  cat(paste("Mediana: ", estadisticos$mediana, "\n"), file = nombre_archivo, append = TRUE)
  cat(paste("Desviación estándar: ", estadisticos$desviacion_estandar, "\n"), file = nombre_archivo, append = TRUE)
  cat(paste("Mensaje de variabilidad: ", estadisticos$mensaje, "\n\n"), file = nombre_archivo, append = TRUE)
  cat("Cuadrados de los números:\n", file = nombre_archivo, append = TRUE)
  cat(paste(cuadrados, collapse = ", "), file = nombre_archivo, append = TRUE)
}

# Main: flujo principal del programa
# Leer el archivo
numeros <- leer_numeros("numeros.txt")

# Calcular los estadísticos
estadisticos <- calcular_estadisticos(numeros)

# Calcular los cuadrados de los números
cuadrados <- calcular_cuadrados(numeros)

# Guardar los resultados en el archivo
guardar_resultados("resultados.txt", estadisticos, cuadrados)

# Imprimir en consola los resultados
print("El análisis ha sido realizado y guardado en resultados.txt")

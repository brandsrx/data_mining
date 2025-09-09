# imputacion multiple
# tambien se puede usar la libreria mice

# Instalar y cargar el paquete mice (si no está instalado)
install.packages("mice")
library(mice)

# Datos originales
datosm <- data.frame(
  Persona = 1:6,
  Edad = c(25, 30, NA, 28, NA, 35)
)

# Aplicar imputación múltiple
# m=5 genera 5 conjunto de datos imputados 
# metodo pmm usa el Predictive Mean Matching (PMM)
# metodo robusto para imputar valores numericos
# maxit = 10 numero max de iteraciones para la convergencia
# complete() combina los conjuntos imputados en un solo dataframe
imp <- mice(datosm, m = 5, method = "pmm", maxit = 10)  # m = 5 conjuntos imputados
datos_imputados <- complete(imp)  # Combinar los conjuntos imputados
# imputacion multiple, refleja la incertidunbre de los valores faltantes
# genera multiples conjuntos de datos, permite un analisis mas robusto
# adecuada para datos con patrones de faltantes complejos
# cuando el patron de MAR (Missing at Random)

# Verificar la convergencia del modelo
plot(imp)  # Gráficos de convergencia
summary(imp)  # Resumen de los modelos de imputación
# muestra evlolucion en la media y desviacion standard para cada variable imputada
# cada linea representa un conjunto de datos imputado (m)
# si las lineas se superponen y se estabilizan, indica que el algoritmo ha convergido



# Instalar y cargar paquetes necesarios
install.packages("mice")
install.packages("VIM")  # para gráficos de datos faltantes
library(mice)
library(VIM)

# Crear el dataframe
datosm <- data.frame(
  Persona = 1:6,
  Edad = c(25, 30, NA, 28, NA, 35)
)

# Visualizar datos faltantes
aggr(datosm, numbers = TRUE, sortVars = TRUE, labels = colnames(datosm), cex.axis = 0.7, gap = 3, ylab=c("Cantidad de datos faltantes", "Patrón"))

# Aplicar imputación múltiple (m=5 imputaciones, maxit=5 iteraciones)
imp <- mice(datosm, m = 5, maxit = 5, method = 'pmm', seed = 123)

# Resumen de la imputación
summary(imp)

# Visualizar las imputaciones realizadas para la variable Edad
stripplot(imp, Edad ~ .imp, pch = 20, cex = 1.2)

# Obtener uno de los datasets imputados completos (ejemplo: el primero)
datosm_completos <- complete(imp, 1)

# Imprimir datos completados
print(datosm_completos)

plot(imp)  # Gráficos de convergencia

# el gráfico,muestra que cada línea representa cómo varían las imputaciones 
# para una variable (en este caso Edad) a lo largo de las iteraciones.
# Si las líneas se estabilizan (se vuelven planas y no cambian mucho), 
# indica que el proceso de imputación ha convergido.
# Esto sugiere que las imputaciones ya no cambian significativamente con
# iteraciones adicionales y, por lo tanto, son más confiables.
# Si las líneas oscilan mucho o tienen tendencias fuertes, 
# puede indicar que el proceso no ha convergido aún y podrían ser necesarias
# más iteraciones o ajustes en el modelo.
# este grafico permite verificar la validez técnica del proceso de imputación múltiple
# y asegurar que los datos imputados tengan estabilidad y coherencia estadística
# si convergen esto es que la imputacion multiple llega a un punto estable
# la convergencia indica estabilidad y confiabilidad de las imputaciones


install.packages("readxl")
library("readxl")

data_apple <- read_excel('aapl_datos.xlsx')
print(data_apple)
# media ,mediana y moda de "close"
mean_apple <- mean(data_apple$Close, na.rm = TRUE)
median_apple <- median(data_apple$Close, na.rm = TRUE)

moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}
moda_apple <- moda(data_apple$Close)

print(paste("Media:", mean_apple, "Moda:", moda_apple, "Mediana:", median_apple))


# Medidas de dispercion
min_max <- range(data_apple$Close, na.rm = TRUE)   # min y max
rango <- diff(range(data_apple$Close, na.rm = TRUE)) #rango
varianza <- var(data_apple$Close, na.rm = TRUE) # varianza
desviacion_estanda <- sd(data_apple$Close, na.rm = TRUE)  # desviacion estandar

print(paste("min y max:",min_max,"range: ",rango,"varianza: ",varianza,"Desviacion_estandar: ",desviacion_estanda))



library(ggplot2)
# Histograma con media, mediana y moda
ggplot(data_apple, aes(x = Close)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_apple), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median_apple), color = "green", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = moda_apple), color = "blue", linetype = "dashed", size = 1.2) +
  labs(title = "Distribución de Precios de Cierre (Close)",
       x = "Precio de Cierre",
       y = "Frecuencia") +
  theme_minimal()

# Boxplot para ver dispersión, outliers, min y max
ggplot(data_apple, aes(y = Close)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot de Precios de Cierre (Close)",
       y = "Precio de Cierre") +
  theme_minimal()

# Densidad para ver la forma de la distribución
ggplot(data_apple, aes(x = Close)) +
  geom_density(fill = "purple", alpha = 0.4) +
  geom_vline(aes(xintercept = mean_apple), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median_apple), color = "green", linetype = "dashed", size = 1.2) +
  labs(title = "Densidad de Precios de Cierre (Close)",
       x = "Precio de Cierre",
       y = "Densidad") +
  theme_minimal()



resumen <- data.frame(
  Min = min_max[1],
  Max = min_max[2],
  Rango = rango,
  Varianza = varianza,
  DesviacionEstandar = desviacion_estanda
)


# Data frame para graficar
resumen_plot <- data.frame(
  Medida = c("Min", "Max", "Rango", "Varianza", "Desviación Estándar"),
  Valor = c(min_max[1], min_max[2], rango, varianza, desviacion_estanda)
)
# Gráfico de barras
ggplot(resumen_plot, aes(x = Medida, y = Valor, fill = Medida)) +
  geom_bar(stat = "identity", width = 0.6) +
  theme_minimal() +
  labs(title = "Medidas de Dispersión (Apple Close)",
       x = "Medida",
       y = "Valor")
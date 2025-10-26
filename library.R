library(ggplot2)
#crear diagrama de barras con ggplot

df <- data.frame(
  id = 1:10,
  edades = c(22, 25, 29, 31, 35, 40, 45, 50, 55, 60)
)
df

# Histograma con media, mediana y moda
hist(df$edades,
     main = "Histograma de edades",
     xlab = "Edad",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "black")


ggplot(data_apple, aes(y = Close)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Boxplot de Precios de Cierre (Close)",
       y = "Precio de Cierre") +
  theme_minimal()


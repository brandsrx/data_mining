# Datos de alturas
alturas <- c(150, 160, 162, 165, 170, 175, 180, 182, 185, 190)
df <- data.frame(altura = alturas)

# Clustering con k-means para 3 clusters
set.seed(123)
km <- kmeans(df, centers = 4)

# Añadir clusters al dataframe
df$cluster <- as.factor(km$cluster)
cuartiles <- quantile(df$altura, probs = c(0, 0.25, 0.50, 0.75, 1))
as.
# Asignar etiquetas basadas en rangos de cluster
df$cluster_label <- cut(df$altura,
                        breaks = 4,
                        labels = c("Bajo", "Medio", "medio Alto","Alto"),
                        right = TRUE)

print(as.numeric(cuartiles))
# Mostrar tabla con altura, cluster numérico, y etiqueta
print(df)

# Instalar y cargar ggplot2 si no está instalado
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Graficar las alturas coloreadas por etiqueta de cluster
ggplot(df, aes(x = altura, y = rep(0, length(altura)), color = cluster_label)) +
  geom_point(size = 5) +
  labs(title = "Clustering de alturas con 3 grupos etiquetados",
       x = "Altura (cm)",
       y = "") +
  theme_minimal() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


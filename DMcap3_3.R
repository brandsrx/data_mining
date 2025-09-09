# Vector de edades
edades <- c(22, 25, 29, 31, 35, 40, 45, 50, 55, 60)

# Número de bins (intervalos) deseados
num_bins <- 4

# Crear factores con intervalos de igual ancho (equal-width binning)
edades_bins <- cut(edades,
                   breaks = num_bins,
                   include.lowest = TRUE,
                   right = TRUE)

# Data frame con edades y sus bins asignados
df <- data.frame(edades = edades, bin = edades_bins)

# Imprimir resultados
print(df)

# Graficar con ggplot2
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x=edades, y=0, color=bin)) +
  geom_point(size=5) +
  labs(title = "Discretización por Equal-Width Binning (3 bins)",
       x = "Edad",
       y = "") +
  theme_minimal() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


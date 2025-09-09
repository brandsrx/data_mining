# -----------------------------------------------------
# Práctica 4: Correlación entre variables numéricas
# Dataset: palmerpenguins
# -----------------------------------------------------

# Instalar y cargar librerías necesarias
install.packages("palmerpenguins")
install.packages("ggplot2")
install.packages("GGally")  # para pairplots y matrices de correlación
library(palmerpenguins)
library(ggplot2)
library(GGally)

# -----------------------------------------------------
# 1. Preparar datos
# -----------------------------------------------------
# Seleccionar solo columnas numéricas y quitar NA
penguins_num <- na.omit(penguins[, c("bill_length_mm", "bill_depth_mm", 
                                     "flipper_length_mm", "body_mass_g")])

# -----------------------------------------------------
# 2. Matriz de correlación
# -----------------------------------------------------
cor_matrix <- cor(penguins_num)
print("Matriz de correlación entre variables numéricas:")
print(round(cor_matrix, 3))

# -----------------------------------------------------
# 3. Visualización: Heatmap de correlaciones
# -----------------------------------------------------
ggcorr(penguins_num, label = TRUE, hjust = 0.75, size = 3, color = "grey50") +
  labs(title = "Mapa de calor de correlaciones entre variables de pingüinos") +
  theme_minimal(base_size = 13)

# -----------------------------------------------------
# 4. Pairplot: todas las relaciones con histogramas
# -----------------------------------------------------
ggpairs(penguins_num,
        title = "Matriz de dispersión y correlaciones de variables numéricas") +
  theme_minimal(base_size = 12)

# -----------------------------------------------------
# 5. Ejemplo práctico: Correlación entre longitud de aleta y peso
# -----------------------------------------------------
ggplot(na.omit(penguins), aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Correlación entre longitud de aleta y peso corporal",
       x = "Longitud de aleta (mm)",
       y = "Peso corporal (g)") +
  theme_minimal(base_size = 13)

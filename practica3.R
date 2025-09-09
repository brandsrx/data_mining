# -----------------------------------------------------
# Práctica 3: Estadística inferencial con palmerpenguins
# -----------------------------------------------------

# Instalar y cargar librerías necesarias
install.packages("palmerpenguins")
install.packages("ggplot2")
install.packages("ggpubr") # gráficos con test estadísticos
library(palmerpenguins)
library(ggplot2)
library(ggpubr)

# Eliminar valores NA para evitar warnings
penguins_clean <- na.omit(penguins)

View(penguins_clean)

# -----------------------------------------------------
# 1. T-test: diferencia de peso entre sexos
# -----------------------------------------------------
t_test_result <- t.test(body_mass_g ~ sex, data = penguins_clean)
print("Resultado del T-test:")
print(t_test_result)

# Gráfico boxplot con medias y p-value
ggboxplot(penguins_clean, x = "sex", y = "body_mass_g",
          color = "sex", palette = "jco",
          add = "jitter", shape = 21) +
  stat_compare_means(method = "t.test", label = "p.format") +
  labs(title = "Comparación de peso entre sexos (t-test)",
       x = "Sexo", y = "Peso corporal (g)") +
  theme_minimal(base_size = 13)

# -----------------------------------------------------
# 3. Chi-cuadrado: relación entre sexo e isla
# -----------------------------------------------------
tabla <- table(penguins_clean$sex, penguins_clean$island)
chi_result <- chisq.test(tabla)
print("Resultado del Chi-cuadrado:")
print(chi_result)

# Gráfico de barras apiladas normalizadas
ggplot(penguins_clean, aes(x = island, fill = sex)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Distribución de sexos según isla (Chi-cuadrado)",
       x = "Isla", y = "Proporción (%)", fill = "Sexo") +
  theme_minimal(base_size = 13)

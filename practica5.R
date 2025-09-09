# Dataset mtcars (incluido en R)
data(mtcars)
head(mtcars)
View(mtcars)


# 1. Modelo de regresión lineal
modelo <- lm(mpg ~ hp, data = mtcars)
summary(modelo)

# 2. Gráfico de dispersión con recta de regresión
plot(mtcars$hp, mtcars$mpg,
     main = "Regresión lineal: Consumo (mpg) vs Potencia (hp)",
     xlab = "Caballos de fuerza (hp)",
     ylab = "Millas por galón (mpg)",
     pch = 19, col = "blue")

abline(modelo, col = "red", lwd = 2)

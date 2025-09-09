# imputacion por regresion

# Instalar paquetes 

install.packages("reformulas")

install.packages("mice", dependencies = TRUE)


install.packages("VIM")  # Alternativa para hot-deck y otros métodos

# Cargar bibliotecas
#library(mice)  # Para imputación múltiple y regresión
library(reformulas)
library(mice)


library(VIM)   # Para métodos como hot-deck y visualización
datos <- data.frame(
  Persona = 1:6,
  Edad = c(25, 30, NA, 28, NA, 35)
)

# Ejemplo con regresión determinística
imp_det <- mice(datos, method = "norm.predict", m = 1, maxit = 1)
datos_imp_det <- complete(imp_det)

# Ejemplo con regresión estocástica
imp_est <- mice(datos, method = "norm.nob", m = 1, maxit = 1)
datos_imp_est <- complete(imp_est)


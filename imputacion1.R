#imputacion Hot- Deck

# Instalar paquete necesario (si no está instalado)
install.packages("VIM")

# Cargar bibliotecas
library(VIM)

# Datos originales
datos <- data.frame(
  Persona = 1:6,
  Edad = c(25, 30, NA, 28, NA, 35)
)

# Aplicar hot-deck con selección aleatoria homogénea
datos_imputados <- hotdeck(
  datos,
  variable = "Edad",
  ord_var = "Persona",  # Variable de ordenamiento
)

# Resultado final
datos_imputados[, c("Persona", "Edad_imp")]


print(datos_imputados)

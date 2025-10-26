#Ejemplo de datos dummy
#Crear dataset con una variable de 3 categorias

df <- data.frame(
  id=1:5,
  color = c("Rojo","Verde","Azul","Verde","Rojo")
)

# se crean variables binarias con model-matrix() para cada categoria
# crear variables dummy elimando la columna intercepto
dummies <- model.matrix(~color-1,data = df)
df_final <- cbind(df,dummies)
print(df_final)

library(dplyr)

# Crear un datafram de ejemplo
df <- data.frame(
  id = 1:100,
  grupo = sample(c("A","B","C"),100,replace = TRUE),
  valor = rnorm(100)
)

#Subsamplaer el conjunto de datos para obtener una muestra
# mas pequeña podemos subsamplear 
# 20 observaciones por grupo (A,B,C)

df_subsampled <- df %>%
  group_by(grupo) %>%
  slice_sample(n=20)

View(df)
#Verificamos el resultado
print(df_subsampled)

# para obtener una cantidad fija por grupi con slice_sample de 20 observaciones por grupo




#imputawicon por Hot DECK
#Instalar paquetes necesario
install.packages("VIM")
library(VIM)

#Datos originales
datos <- data.frame(
  Persona = 1:20,
  Edad = c(25,30,NA,28,NA,35,18,20,NA,30,33,NA,40,41,38,NA,NA,25,27,30)
)

datos_imputados <- hotdeck(
  datos,
  variable = "Edad",
  ord_var = "Persona",
)

datos_imputados[, c("Persona","Edad_imp")]


print(datos_imputados)





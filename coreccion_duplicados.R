#ejemplo de deteccion y coreccion de duplicados
library(dplyr)

#crear un dataset
datos <- data.frame(
  ID = 1:8,
  Nombre = c("Ana","Juan","Ana","Luis","Juan","Ana","Maria","Luis")
)

#identificar duplicados en el nombre
duplicados <- datos[duplicated(datos$Nombre),]
print(duplicados)

#contar duplicados 
num_duplicados <- sum(duplicated(datos$Nombre))
print(num_duplicados)

# Correccion de duplicados
#Eliminar datos duplicados completos
datos_sin_duplicados <- datos[!duplicated(datos$Nombre),]
print(datos_sin_duplicados)

#mantener solo la primera ocurrencia
datos_unicos <- datos %>%
  distinct(Nombre,.keep_all = TRUE)
print(datos_unicos)
#mantaer duplicados bajo condicion (ejemp, mantener id mas alto)
datos_condicion <- datos %>%
  group_by(Nombre) %>%
  filter(ID == max(ID)) %>%
  ungroup()
print(datos_condicion)


# deteccion y coreccion de datos incompletos,ejemplo
# ejemplo de datos incompletos
#crear un dataset
datos <- data.frame(
  ID = c(1,2,3,4,5),
  Edad = c(25,NA,30,28,NA),
  Nombre = c("Ana","Juan",NA,"Luis","Maria")

#identificar valores faltantes
#verificar valores faltantes en todo el dataframe
is.na(datos)
#contar valores faltantes por columnsa
colSums(is.na(datos))

#resumen de valores faltantes
summary(datos)

#Correccion de datos incompletos

#liminar filas con valores faltantes
datos_completos
)
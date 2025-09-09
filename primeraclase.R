# Datos nominales: colores favoritos

colores <- factor(c("rojo", "azul", "verde", "azul", "rojo", "verde", "amarillo","azul"))
# Datos ordinales: calificaciones con orden establecido
calificaciones <- factor(c("bueno", "excelente", "regular", "muy bueno", "bueno", "regular"),
                         levels = c("regular", "bueno", "muy bueno", "excelente"),
                         ordered = TRUE)
# Tabla de frecuencias nominales
tabla_colores <- table(colores)
print(tabla_colores)
# Tabla de frecuencias ordinales
tabla_calificaciones <- table(calificaciones)
print(tabla_calificaciones)
# Gráfico de barras para datos nominales
barplot(tabla_colores, main = " Frecuencia de colores favoritos", col = 
        rainbow(length(tabla_colores)),
        ylab = "Frecuencia", xlab = "Colores")
# Gráfico de barras para datos ordinales
barplot(tabla_calificaciones, main = "Frecuencia de calificaciones",
        col = heat.colors(length(tabla_calificaciones)),
        ylab = " Frecuencia", xlab = "Calificaciones")
# Gráfico de barras ordenado para ordinales (por orden definido)
library(ggplot2)
df_calificaciones <- data.frame(calificaciones)
ggplot(df_calificaciones, aes(x = calificaciones)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Frecuencia de calificaciones ordinales") +
  xlab("Calificaciones") + ylab("Frecuencia")




#ejercicio en clases
edades <- c(22,21,19,15,21,22,20,22,21,20,19)
sexo <- c("M","M","F","M","M","F","M","F","M","M","F")

datos <- data.frame(edades, sexo)
print(datos)

tabla_edades <- table(datos$edades)

barplot(tabla_edades,
        main = "Cantidad de personas por edad",
        xlab = "Edad",
        ylab = "Cantidad",
        col = "steelblue")

tabla_sexo_edades <- table(datos$edades, datos$sexo)

barplot(datos$edades,datos$sexo,main="personas por edad y sexo")

barplot(tabla_sexo_edades,
        main = "Cantidad de personas por edad y sexo",
        xlab = "Edad",
        ylab = "Cantidad",
        col = c("skyblue","pink"),
        legend = rownames(tabla_sexo_edades),
        beside = TRUE)



# ejercicio 2
# Cargar libreria ggplot2 para graficos avanzados
library(ggplot2)
#Datos cuantitativos discretos: numero de hijos por familia
hijos <- c(1,2,0,3,2,1,4,0,1,2)
#Datos cuantitativos continuos estaturas en cm
estaturas <- c(170.5,165.2,180.3,175.0,169.8,173.4,182.1,160.0,177.8)
#Visualizacion para datos discretos
#Tabla de frecuencias
tabla_hijos <- table(hijos)
print(tabla_hijos)
#Grafico de barras para datos discretos
barplot(tabla_hijos,
        xlab = "Numero de hijos",
        ylab = "Frecuencia",
        col = "lightblue")

# Usando ggplo2 para barras (discretos)
df_hijos <- data.frame(hijos = factor(hijos))
ggplot(df_hijos, aes(x = hijos)) +
         geom_bar(fill = "skyblue") +
         ggtitle("Frecuencia de numeros de hijos") +
         xlab("Numero de hijos") +
         ylab("Conteo")
#Visualizacion para datos continuos
#historama
hist(estaturas,
     main = "Histograma de estaturas",
     xlab = "Estaturas (cm)",
     col = "lightgreen",
     border = "black",
     breaks = 5)

#BoxPlot para datos continuos
boxplot(estaturas,
        main = "Diagrama de caja de estaturas",
        ylab = "Estatura(cm)",
        col = "lightgreen")


# ejercicio 3
sexo <- c("home","hombre","hombre","hombre","mujer","mjuer","mujer","mujer")
edad <- 21:30
id <- 1:10
# crear una base de datos
bd1 <- data.frame(id,edad,sexo)
bd1 <- data.frame(identify=id,edades=edad,sexos=sexo)
#promedio y desv standard (medidad de dispersion)
mean(bd1$edades)
sd(bd1$edades) 
# importando datos
library("dplyr")
install.packages("rstatix")
library("rstatix")
install.packages("ggpubr")
library("ggpubr")

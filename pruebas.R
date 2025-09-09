# Cargar el dataset iris (viene por defecto en R)
data(iris)

# Crear un histograma de Sepal.Length
hist(iris$Sepal.Length,
     main = "Histograma de Sepal.Length",
     xlab = "Longitud del Sépalo (cm)",
     ylab = "Frecuencia",
     col = "skyblue",
     border = "white")

hist(iris$Petal.Length,
     main = "Histograma de Petal length",
     xlab = "Longitu del petalo (cm)",
     ylab = "Frecuencia",
     col = "green",
     border = "black")

plot(iris$Sepal.Length,iris$Petal.Length)

# Crear el boxplot
library(ggplot2)
ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot() +
  labs(title = "Comparación de la Longitud del Sépalo por Especie",
       x = "Especie",
       y = "Longitud del Sépalo (cm)") +
  theme_minimal()
unique(iris$Species)
# calcular el promedio de cada especie 
setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]
virginica <- iris[iris$Species == "virginica",]
print(paste("promedio de setosa length", mean(setosa$Sepal.Length)))
print(paste("Promedio de versicolor ",mean(versicolor$Sepal.Length)))
print(paste("Promedio de virginica ",mean(virginica$Sepal.Length)))

#manipulacion de datos
library(dplyr)
data("iris")
#filtrar flores con Petal.length
iris %>% filter(Petal.Length > 1.5)
#Agrupar y calcular media
iris %>% group_by(Species) %>% summarise(Promedio = mean(Sepal.length))


#Visualizacion de datos
library(ggplot2)
ggplot(iris,aes(x = Sepal.Length,y = Sepal.Width,color = Species))+
  geom_point(size = 3)+
  theme_minimal()



# Estadistica aplicada
cor(mtcars$mpg,mtcars$hp)
modelo <- lm(mpg ~ hp,data = mtcars)
summarise(modelo)

# ejemplo de discretizacion

install.packages("dplyr")
library("dplyr")

install.packages("readxl")
library("readxl")

install.packages("rstatix")
library("rstatix")

install.packages("ggpubr")
library("ggpubr")

id <- 1:10
estatura <- c(150,160,165,170,175,180,185,190,172,171)


# crear una base de datos
bd1 <- data.frame(id,estatura)
bd1

summary(bd1$estatura)

summary(bd1)
median(bd1$estatura)
#creamos un campo para la discre
bd1['discre'] <- cut(bd1$estatura,
                     breaks = c(min(bd1$estatura), median(bd1$estatura), max(bd1$estatura)),
                     labels = c('bajo','alto'),
                     include.lowest = TRUE)

head(bd1)

cuartiles <- quantile(estatura, probs = c(0, 0.25, 0.50, 0.75, 1))

summary(cuartiles)
print(as.numeric(cuartiles))
#visualizamos
plot(bd1$discre)
bd1['discre'] <- cut(bd1$estatura,
                     breaks = as.numeric(cuartiles),
                     labels = c('bajo','mediano','alto','muy alto'),
                     include.lowest = TRUE)

# bajo, normal, alto


data2 <- read.csv('obe.csv')
df <- data.frame(data2)
summary(data2)


data2$cat_edad <- cut(
  data2$Edad,
  breaks = 4,
  labels = c("Joven","Adulto_joven","Adulto","Mayor")
)

summary(data2)
summary(data2$CH2O_cat)
plot(data2$cat_edad)

data2 <- data2 %>%
  group_by(cat_edad) %>%
  mutate(CH2O_cat = ntile(CH2O, 3)) %>%  
  ungroup()




data2$CH2O_cat <- factor(data2$CH2O_cat, labels = c("Bajo","Medio","Alto"))


ggplot(data2, aes(x = cat_edad, y = CH2O_cat)) +
  geom_point(size = 5) +
  labs(title = "Consumo de Agua por Grupo de Edad",
       x = "Grupo de Edad",
       y = "Consumo de Agua") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.ticks.y = element_blank())


data2$CH2O_cat <- factor(data2$CH2O_cat, levels = c("Bajo","Medio","Alto"))

# Tabla cruzada Edad vs Consumo de agua
table(data2$cat_edad, data2$CH2O_cat)


ggplot(data2, aes(x = cat_edad, fill = CH2O_cat)) +
  geom_bar(position = "fill") +   # posición "fill" para proporciones
  labs(title = "Proporción de consumo de agua por grupo de edad",
       x = "Grupo de Edad", 
       y = "Proporción", 
       fill = "Consumo de Agua") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
sexo <- c("hombre","hombre","hombre","hombre","hombre","mujer","mujer","mujer","mujer","mujer")
sexo
edad <- 21:30
edad
id <- 1:10
id

# crear una base de datos
bd1 <- data.frame(id,edad,sexo)
bd1 <- data.frame(identify=id,edades=edad,sexos=sexo)
bd1

#promedio y desv standard (medida de dispersion)
mean(bd1$edades)  #25.5
sd(bd1$edades)    #3.02



# importando datos
install.packages("dplyr")
library("dplyr")

install.packages("readxl")
library("readxl")

install.packages("rstatix")
library("rstatix")

install.packages("ggpubr")
library("ggpubr")

#leer doc exccel en R
db2 <- read_excel("ejem1.xlsx")
db2
View(db2)



#modificar base de datos
# ordenar datos de edad asc

db2

db2 %>%
  arrange(edad) %>% 
  View()

      #otro formato
db2A <- arrange(db2, edad)
View(db2A)


# descendente
db2 %>%
  arrange(desc(edad)) %>% 
  View()

    # otro codigo sin pipe
db2B <- arrange(db2, desc(edad))
View(db2B)


#filtrar datos el pipe son atajos

db2 %>% 
  filter(edad > 21)
      # sin pipe
dbm21 <- filter(db2, edad > 21)

db2 %>% 
  filter(edad %in% c(19,21))

db2 %>% 
  filter(sexo == "hombre")

db2 %>% 
  filter(estrés_cat == "alto")

db2 %>% 
  filter(estrés_cat %in% c("bajo","alto"))

# seleccionar variables especificas
db2 %>% 
  select(id, sexo, estrés)

# cambiar nombre de variables

db2 %>% 
  rename(age=edad, sex=sexo, mental_agy = agilidad,
         stress= estrés, stress_cat = estrés_cat)


db2$agilidad_cat <- cut(db2$agilidad,
                        breaks = c(0, 10, 20, 30, 50),
                        labels = c("bajo", "regular", "mediano", "alto"),
                        ordered = TRUE)
db2

#correlacion entre variables

db2
db2 %>% 
  cor_test(agilidad,estrés)

# relacion negativa mientras,aumenta estres disminuye agilidad mental

ggplot(data = db2, aes(x=estrés, y = agilidad))+
  geom_point() 

ggplot(data = db2, aes(x=estrés, y = agilidad))+
  geom_point() +
  geom_smooth()


ggplot(data = db2, aes(x=estrés, y = agilidad))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = db2, aes(x=estrés, y = agilidad))+
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)


ggplot(data = db2, aes(x=estrés, y = agilidad))+
  geom_point() +
  geom_smooth(method = "lm", se=FALSE)+
  theme_pubr()+
  labs(x="ESTRES", y="AGILIDAD MENTAL")



#diferencia entre grupos, porejemplo ANOVA que es analisis de varianza

db2

db2 %>% 
  anova_test(agilidad ~ estrés_cat)
# si el p<0.05 hay diferencia entre los puntajes, entre agilidad
# mental entre participantes que tienen alto bajo mediano stres
# este no es el caso


# visualizamos, diagrama de caja

# Resumen de la variable estrés
summary(db2$estrés)

# Contar NA y categorías en estrés_cat
table(db2$estrés_cat, useNA = "always")

# Contar NA en agilidad
sum(is.na(db2$agilidad))

ggboxplot(data = db2, x="estrés_cat",
          y="agilidad")
db2$estrés_cat <- cut(db2$estrés,
                      breaks = c(-Inf, 0, 10, 20, 30, 56),
                      labels = c("fuera de rango","bajo", "regular", "mediano", "alto"),
                      ordered = TRUE)
#esta desordenado 
ggboxplot(data = db2, x="estrés_cat",
          y="agilidad", order=c("bajo","regular","mediano","alto"))

#con colores
ggboxplot(data = db2, x="estrés_cat",
          y="agilidad", order=c("bajo","regular","mediano","alto"),
          color = "estrés_cat")
View(db2)
# con relleno

ggboxplot(data = db2, x="estrés_cat",
          y="agilidad", order=c("bajo","regular","mediano","alto"),
          fill = "estrés_cat")

#otro grafico
ggviolin(data = db2, x="estrés_cat",
         y="agilidad", order=c("bajo","regular","mediano","alto"),
         fill = "estrés_cat")

#combinar buscar otros formatos
ggviolin(data = db2, x="estrés_cat",
         y="agilidad", order=c("bajo","regular","mediano","alto"),
         fill = "estrés_cat",
         add = "boxplot", add.params = list(fill="white"))


table_sexo <- table(db2$sexo)
df_sexo <- as.data.frame(table_sexo)
# Por defecto, las columnas se llaman Var1 (categoría) y Freq (frecuencia)
ggplot(df_sexo, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_col() +
  labs(x = "Sexo", y = "Frecuencia", fill = "Sexo") +
  theme_grey()

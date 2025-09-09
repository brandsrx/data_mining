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

#creamos un campo para la discre
bd1['discre'] <- cut(bd1$estatura, breaks = c(0,150,160,170,180,190,200),
                     labels = c('0-150','151-160','161-170','171-180','181-190','191-200'))

head(bd1)

#visualizamos
plot(bd1$discre)

#creamos un campo para la discre categorizada
bd1$discreCat <- cut(bd1$estatura,
                        breaks = c(0, 150, 160, 170, 180, 190),
                        labels = c("bajo","regular","normal","alto","muy alto"),
                        ordered = TRUE)



plot(bd1$discreCat)

# bajo, normal, alto

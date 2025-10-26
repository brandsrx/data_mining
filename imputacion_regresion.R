#imputacion por regrsion
#instalar paquetes
install.packages("reformulas")
install.packages("mice",dependencies = TRUE)
install.packages("VIM")

#cargar bibliotcas
library(reformulas)
library(VIM)
library(mice)

datos <- data.frame(
  Persona = 1:6,
  Edad = c(25,30,NA,28,NA,35)
)

# ejemplo con regresion deterministica
imp_det <- mice(datos,method = "norm.predict",m = 1,maxit = 1)
datos_imp_det <- complete(imp_det)

#ejemlo con reresion estocastica
imp_ext <- mice(datos,ethod = "norm.nob",m = 1,maxit = 1)
datos_imp_det <- complete(imp_det)

#ejemplo con regresion estocastica
imp_est <- mice(datos,ethod = "norm.nob", m = 1,maxit = 1)
datos_imp_est <- complete(imp_est)


#Imputacon multiple
#Tambien se puedes usar la librerai mice
#incstalar y cargar el parquete mice ( si no esta instalado )
datosm <- data.frame(
  Persona = 1:6,
  Edad = c(25,30,NA,28,NA,35)
)

#visualizamos datos faltantes
aggr(datosm,numbers = TRUE, sortVars = TRUE, labels = colnames(datosm),cex.axis = 0.7, gap = 3, ylab = c("Cantidad de datos faltantes","patron"))

#Aplicar imputacion multiple
# m=5 genera 5 conjuntos de datos imputados
#metodo pmmm usa el PRedictive Mean Matching (PMM)
#maxit = 10 numero max de iteracion para la convergencia
#complete() combina los conjuntos imputados en un solo data.frame
imp <- mice(
  datosm,
  m = 20,              # número de datasets imputados que se van a generar
  method = "pmm",     # método Predictive Mean Matching (recomendado para datos numéricos)
  maxit = 15,           # número de iteraciones para lograr convergencia
  seed = 120           # semilla para reproducibilidad
)
datos_imputados <- complete(imp)
print(datos_imputados)
#imputacion multiple regleja la icertidumbre de los valores faltantes
#genera multiples refleja la incertidumbre de los valores faltantes 
#genera multiples conjuntos de datos permite una analisis mas robuto
#adecuada para datos con patrones de faltantes complejos
#cuando el patron de mAR(missing at Random)
stripplot(imp, Edad ~ .imp, pch = 20, cex = 1.2)

#Verificar la convergencia del modelo
plot(imp)
summary(imp)

#muestra evolucion en la media desviacion standar para cada variables umputada
#cada linea representa un conjunto de datos imputados (m)
#si las lineas se superponen y se estabilizan indica que algoritmo a convergido

library(readxl)
file.exists("/home/brandon/Documents/data_mining/datasets/Tiquina.xls")
df <- read_xlsx("datasets/Tiquina.xls",skip=4, sheet = "CODP3")
View(df)
colSums(is.na(df))
aggr(df, numbers = TRUE, sortVars = TRUE)
median(df$`3`,na.rm = TRUE)


# Seleccionamos solo columnas numéricas para imputar
datos_num <- df[, sapply(df, is.numeric)]
variables <- colnames(df)
print(variables[3:length(variables)])
datos_imputados <- hotdeck(
  df,
  variable = variables[3:length(variables)],
  ord_var = "AÑO",  # Variable de ordenamiento
)
View(datos_imputados)

# Combinar los resultados imputados en un solo dataset completo
df_imputado <- complete(imp)

aggr(df, numbers = TRUE, sortVars = TRUE)
aggr(datos_imputados, numbers = TRUE, sortVars = TRUE)

# Antes de imputar
hist(df$`1`, main = "Antes de imputar", xlab = "Temp día 1")

# Después de imputar
hist(datos_imputados$`1`, main = "Después de imputar", xlab = "Temp día 1")

boxplot(df$`1`, datos_imputados$`1`,
        names = c("Antes","Después"),
        main = "Comparación de valores imputados")


library(dplyr)
library(tidytext)

#Texto 2 con bigramas en varias frases
texto2 <- data.frame(
  id = 1:10,
  contenido = c("La politica en Bolivia pone triste a la gente",
                "El partido para el mundial una pena",
                "Las marchas son el pan da cada dia",
                "Estan pensando subir el costo del pan",
                "Los transportisas amenzan cada dia",
                "Los transportistas quieren entrar en paro por el problema del gas",
                "estamos entrando en un frente frio",
                "cada dia suben las cosas",
                "el pollo otra vez ha subido",
                "de todos modos subio el pasaje"
                )
  )

#Tokenizacion simle a nivel de palabra
tokens_palabras <- texto2 %>%
  unnest_tokens(palabra,contenido)

print("Tokens por palabra")
print(tokens_palabras)

#Tokenizacion a nivvel de bigrama (pares de palabras)
tokens_bigrama <- texto2 %>%
  unnest_tokens(bigrama,contenido,token = "ngrams", n =3)
print("Tokens por bigramas:")
print(tokens_bigrama)


#ejemplo 4 de tokenizacion de caracteres
 # texto de ejemplo 
texto <- "estamos listos para trabajar"

#Tokenizacion de caracteres ( seprara en vector de caracteres)
caracteres <- unlist(strsplit(texto,split= ""))
#Contar frecuencia de cada caracer
tabla_frecuencias <- table(caracteres)

#Mostrar tabla de frecuencias 
print(tabla_frecuencias)

#graficar frecuencia de caracteres
barplot(tabla_frecuencias,
        main = "Frecuencia de caracteres en la frase",
        xlab = "Caracteres",
        ylab = "Frecuencia",
        col = "skyblue")




#Ejemplo 5 traduce caracter por caracter a un nuevo formato
library(ggplot2)
library(tidyr)

#Texto en espaol de ejemplo
text <- "hola mundo"
#Tokenizacion por caracteres
caracteres <- unlist(strsplit(texto,split = ""))

#crear datafrane con la tokenizacion
df <- data.frame(caracteres = caracteres, stringsAsFactors = FALSE )

# Ejemplo didactico de "Traduccion " de caracteres a simboos (simulacion)

traduccion <- c("h" = "H","o" = "O","l"="L","a"="A",
                " "=" ","m"="M","u"="U","n"="N","d"="D")

df$token_traducido <- sapply(df$caracteres,function(x) traduccion[x])

#Contar frecuencia de caracteres y tokens traducidos
freq_original <- df %>% count(caracteres) %>% rename(token = caracteres,frecuencia = n)
freq_traducido <- df %>% count(token_traducido) %>% rename(token = token_traducido,frecuencia = n)

#Graficar frecuencias original y traducida
freq_total <- rbind(
  freq_original %>% mutate(tipo = "Original"),
  freq_traducido %>% mutate(tipo = "Traducido")
)

ggplot(freq_total,aes(x = token, y = frecuencia, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Frecuencia de caracteres y tokens traducidos",
       x = "Token",
       y = "Frecuencia")+
  scale_fill_manual(values = c("Original" = "steelblue","Traducido"="tomato"))+
  theme_minimal()



#Ejemplo 6
#Texto de ejemplo 
#calcula la frecuencia por palabra
texto <- "caminante no hay camino se hace camino al andar ,camino al andar, al andar se hace camino
caminanate no hay camino si no sendas al mar"
#Tokenizacion por labras
palabras <- unlist(strsplit(tolower(texto),"\\W+"))
#Crear dataframe con frecuencia de palabras
df <- data.frame(palabras = palabras,stringsAsFactors = FALSE) %>%
  filter(palabras != "") %>%
  count(palabras, sort = TRUE)

#Mostrar tabla de frecuencias
print(df)

df_top <- head(df,5)
ggplot(df_top,aes(x = reorder(palabras,n), y = n, fill = palabras))+
  geom_bar(stat = "identity")+
  coord_flip() +
  labs(title = "Palabras mas frecuentes",
       x = "Palabras",
       y = "Frecuencia")+
  theme_minimal()+
  theme(legend.position = "none")


# chat bot
library(stringr)
#Base sencilla de preguntas frecuentes
 chat_data <- data.frame(
   pregunta = c("hola","bien","haces","adios"),
   respuesta = c("Hola, ¿ en que puedo ayudarte'",
                 "Estoy bien gracias por preguntar",
                 "Estoy aprendiendo a hacer chatbots",
                 "Adios que tengas buen dia"),
   stringsAsFactors = FALSE
 )

 # Función para tokenizar texto en palabras
 tokenizar <- function(texto) {
   texto <- tolower(texto)                     # pasar a minúsculas
   palabras <- unlist(strsplit(texto, "\\W+")) # separar por no-palabras
   palabras <- palabras[palabras != ""]        # quitar vacíos
   return(palabras)
 }

install.packages("maps")
install.packages("syuzhet")
install.packages("RColorBrewer")
install.packages("ggplot2")

#Ejemplo 8. identificar lugares en una frase
#(este ejemplo se hizo de forma manual)

library(dplyr)
library(ggplot2)
library(maps)

# Frases con lugares
frases <- c("Voy a Madrid mañana", "La oficina está en Nueva York", "Visitaré Buenos Aires")

# Lugares extraídos manualmente para este ejemplo
lugares <- c("Madrid", "New York", "Buenos Aires")

# Coordenadas aproximadas de los lugares (longitud, latitud)
coord <- data.frame(
  lugar = lugares,
  long = c(-3.7038, -74.0060, -58.4173),
  lat = c(40.4168, 40.7128, -34.6118)
)

# Mapa mundial (simplificado)
mapa <- map_data("world")

# Graficar mapa y señalar lugares
ggplot() +
  geom_polygon(data = mapa, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = coord, aes(x = long, y = lat), color = "red", size = 4) +
  geom_text(data = coord, aes(x = long, y = lat, label = lugar), vjust = -1.2, size = 4) +
  coord_fixed(1.3) +
  labs(title = "Lugares identificados en frases",
       x = "Longitud",
       y = "Latitud") +
  theme_minimal()


#Ejemplo 9. análisis de sentimiento en una frase en ingles

# Instalar paquetes si es necesario
# install.packages("syuzhet")
# install.packages("RColorBrewer")
# install.packages("ggplot2")

library(syuzhet)
library(RColorBrewer)
library(ggplot2)

# Texto de ejemplo en español
#texto <- "Estoy muy feliz y contento hoy, pero también un poco cansado y triste por algunos problemas."
texto <- "What could it be, Paul?
'There's something In the way she moves'
I start think of what attracted me that's all
Just say whatever comes into your head each time
'Attracts me like a cauliflower', until you get the word
Yeah, but I've been through this one like for about six months
You haven't had 15 people joining in, though, no
I mean, just that line, I couldn't think anything like a
'Attracts me like a pomegranate''
(Something in the way) we could have that, attracts me like a pomegranate
Something in the way she moves
Attracts me like a pomegranate
I think it's not good (cauliflower!)
Something in the way she woos me
I don't want to leave her now
You know I believe and how
You know it's that one mic
'Cause if you try and make sense of it, every time
And every line, then you've got to stop every time
We'd just go on and go on, and then go back over it, yeah
You're done, even the line here"
# Obtener valores de sentimientos y emociones usando NRC lexicon
sentimientos <- get_nrc_sentiment(texto)

# Mostrar frecuencias por emoción
print(sentimientos)
# Sumar valores para cada emoción
sentimientos_totales <- colSums(sentimientos[, 1:8])

# Calcular porcentajes
porcentajes <- round(100 * sentimientos_totales / sum(sentimientos_totales), 1)
# Ordenar sentimientos de mayor a menor
orden <- order(sentimientos_totales, decreasing = TRUE)
sentimientos_ordenados <- sentimientos_totales[orden]
porcentajes_ordenados <- porcentajes[orden]

# Crear gráfico de barras con porcentajes
bar_positions <- barplot(sentimientos_ordenados,
                         col = brewer.pal(8, "Set3"),
                         main = "Análisis de Sentimientos (something) sin limpiar",
                         ylab = "Frecuencia",
                         ylim = c(0, max(sentimientos_ordenados) + 2),
                         las = 2)

# Agregar etiquetas de porcentajes arriba de cada barra
text(x = bar_positions,
     y = sentimientos_ordenados + 1,
     labels = paste0(porcentajes_ordenados, "%"),
     cex = 0.9, col = "black")



library(syuzhet)
library(RColorBrewer)

texto <- "Fuiste tú
Tenerte fue una foto tuya puesta en mi cartera
Un beso y verte hacer pequeño por la carretera
Lo tuyo fue la intermitencia y la melancolía
Lo mío fue aceptarlo todo porque te quería
Verte llegar fue luz, verte partir un blues
Fuiste tú
De más está decir que sobra decir tantas cosas
O aprendes a querer la espina o no aceptes rosas
Jamás te dije una mentira o te inventé un chantaje
Las nubes grises también forman parte de paisaje
Y no me veas así, si hubo un culpable aquí
Fuiste tú
Que fácil fue tocar el cielo la primera vez
Cuando los besos fueron el motor de arranque
Que encendió la luz que hoy se desaparece
Así se disfraza el amor para su conveniencia
Aceptando todo sin hacer preguntas
Y dejando al tiempo la estocada a muerte
Nada más que decir
Sólo queda insistir
Dilo
Fuiste tú
La luz de neón del barrio sabe que estoy tan cansada
Me ha visto caminar descalza por la madrugada
Estoy en medio del que soy y del que tú quisieras
Queriendo despertar pensando como no quisiera
Y no me veas así, si hubo un culpable aquí
Fuiste tú
Que fácil fue tocar el cielo la primera vez
Cuando los besos fueron el motor de arranque
Que encendió la luz que hoy se desaparece
Así se disfraza el amor para su conveniencia
Aceptando todo sin hacer preguntas
Y dejando al tiempo la estocada a muerte
Nada más que decir
Sólo queda insistir
Fuiste tú
Que fácil fue tocar el cielo la primera vez
Cuando los besos fueron el motor de arranque
Que encendió la luz que hoy se desaparece
Así se disfraza el amor para su conveniencia
Aceptando todo sin hacer preguntas
Y dejando al tiempo la estocada a muerte
Nada más que decir
Si quieres insistir
Fuiste tú"

# Dividir texto en palabras
palabras <- unlist(strsplit(texto, "\\s+"))

# Obtener sentimientos con diccionario NRC en español
sentimientos_df <- get_nrc_sentiment(palabras, lang = "spanish")

# Sumar frecuencias de cada emoción
frecuencia_sentimientos <- colSums(sentimientos_df[, 1:8])



# Sumar frecuencias de cada emoción (solo las 8 emociones)
frecuencia_sentimientos <- colSums(sentimientos_df[, 1:8])

# Calcular porcentajes
total <- sum(frecuencia_sentimientos)
porcentajes <- (frecuencia_sentimientos / total) * 100

# Crear dataframe para ggplot
df_sentimientos <- tibble(
  emocion = names(frecuencia_sentimientos),
  frecuencia = as.numeric(frecuencia_sentimientos),
  porcentaje = as.numeric(porcentajes)
) %>%
  mutate(
    # Traducir emociones al español
    emocion_es = case_when(
      emocion == "anger" ~ "Ira",
      emocion == "anticipation" ~ "Anticipación",
      emocion == "disgust" ~ "Disgusto",
      emocion == "fear" ~ "Miedo",
      emocion == "joy" ~ "Alegría",
      emocion == "sadness" ~ "Tristeza",
      emocion == "surprise" ~ "Sorpresa",
      emocion == "trust" ~ "Confianza"
    ),
    # Ordenar por frecuencia
    emocion_es = fct_reorder(emocion_es, frecuencia)
  )

# ============================================
# VISUALIZACIÓN 1: BARRAS CON PORCENTAJES
# ============================================

# Paleta de colores personalizada
colores_emociones <- c(
  "Ira" = "#E74C3C",
  "Disgusto" = "#8E44AD",
  "Miedo" = "#34495E",
  "Tristeza" = "#3498DB",
  "Alegría" = "#F39C12",
  "Anticipación" = "#1ABC9C",
  "Sorpresa" = "#E67E22",
  "Confianza" = "#27AE60"
)

p1 <- ggplot(df_sentimientos, aes(x = emocion_es, y = porcentaje, fill = emocion_es)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%\n")),
            hjust = -0.1,
            size = 4,
            fontface = "bold",
            color = "#2C3E50") +
  scale_fill_manual(values = colores_emociones) +
  coord_flip() +
  labs(
    title = "Análisis de Emociones - Diccionario NRC",
    subtitle = "Canción: 'Fuiste Tú' | Sin limpiar",
    x = NULL,
    y = "Porcentaje (%)",
    caption = "Análisis con syuzhet | Diccionario NRC en español"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, color = "#7F8C8D", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "#95A5A6", margin = margin(t = 15)),
    axis.text.y = element_text(size = 12, face = "bold", color = "#34495E"),
    axis.text.x = element_text(size = 10, color = "#7F8C8D"),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#ECF0F1", size = 0.5),
    plot.margin = margin(20, 30, 20, 20)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.2)),
    labels = function(x) paste0(x, "%")
  )

print(p1)

 


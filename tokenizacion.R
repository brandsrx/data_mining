# Ejemplo 1
install.packages("tidytext")
library(tidytext)
library(dplyr)
texto <- data.frame(
  id = 1:3,
  contenido = c("Hola es una prueba de frase",
                "para tokenizar con un ejemplo",
                "en RStudio y puedas ver claro"
                )
  )


tokens <- texto %>%
  unnest_tokens(palabra,contenido)

 print(tokens)
 
 # ejemplo 2
texto <- data.frame(
   id = 1:10,
   contenido = c("La politica en Bolivia pone triste a la gente",
                 "El partido para el mundial una pena",
                 "Las marchas son el pan da cada dia",
                 "Estan pensando subir el costo del pan",
                 "Los transportistas amenzan cada dia",
                 "Los transportistas quieren entrar en paro",
                 "estamos entrando un frente frio",
                 "cada dia suben las cosas",
                 "el pollo otra vez ha subido",
                 "de todos modos subio el pasaje"
                 )
  )
 
tokenizado <- texto %>% unnest_tokens(palabra,contenido)
print(tokenizado) 

tokenizado %>% count(palabra, sort = TRUE)

# Diccionario simple
positivas <- c("feliz", "alegria", "bueno", "excelente", "amor", "sonrisa")
negativas <- c("triste", "pena", "malo", "odio", "enojo", "problema", "costo", "subir", "paro")

# Clasificar cada palabra
tokenizado$sentimiento <- ifelse(tokenizado$palabra %in% positivas, "positivo",
                                 ifelse(tokenizado$palabra %in% negativas, "negativo", "neutral"))

print(tokenizado)

tabla_sentimientos <- table(tokenizado$sentimiento)
print(tabla_sentimientos)



texto <- data.frame(
  id = 1:10,
  contenido = c(
    "Expocruz 2025 se proyecta como un espacio que sigue creciendo y evolucionando para conectar a Bolivia con el mundo, sin perder su esencia y fomentando la innovación en cada sector.",
    "Queremos que cada versión sea más sostenible, un lugar donde los emprendedores, productores y empresas puedan generar oportunidades concretas de negocio y expandir sus mercados.",
    "Nuestro objetivo es consolidar la feria como un referente regional y global, inspirando a las nuevas generaciones, impulsando la economía y mostrando al país como un espacio de desarrollo y progreso.",
    "La participación de 34 países este año refuerza la proyección de Expocruz y evidencia la confianza internacional que existe en Bolivia como mercado y socio estratégico.",
    "Expocruz se consolida como un puente que une culturas, abre oportunidades, comparte visiones y permite la interacción entre distintos sectores productivos del país y del extranjero.",
    "Santa Cruz se reafirma como una ciudad de ferias y eventos de alcance internacional, demostrando capacidad de organización y liderazgo en la región latinoamericana.",
    "Todo esto es posible gracias al esfuerzo colectivo de miles de personas que, con talento, dedicación y compromiso, hacen de la feria un escenario de excelencia y aprendizaje.",
    "El talento y la dedicación de los participantes impulsan la innovación y fortalecen la proyección de Bolivia como un país confiable y competitivo en el ámbito internacional.",
    "El compromiso de la gente, junto con la planificación estratégica y la cooperación institucional, genera progreso económico, cultural y social durante la feria.",
    "Expocruz conecta sectores, genera oportunidades concretas, fortalece la economía y proyecta a Bolivia como un mercado confiable y estratégico en la región y el mundo."
  ),
  stringsAsFactors = FALSE
)


tokenizado <- texto %>% unnest_tokens(palabra,contenido)
print(tokenizado) 
stop_words <- c("la","el","los","las","de","y","como","se","del","un","una","lo","en","al","para","con","a","su","del","por","que","como")

# Filtrar stop words
tokenizado <- tokenizado[!tokenizado$palabra %in% stop_words, ]


# Diccionario simple
# Intensidad de sentimiento
muy_positivo <- c("excelente", "innovación", "liderazgo", "crecimiento", "logro")
positivo <- c("confianza", "proyección", "talento", "oportunidades")
neutral <- c("evento", "país", "feria", "participación")
negativo <- c("costo", "paro", "problema")
muy_negativo <- c("crisis", "conflicto", "peligro")

tokenizado$sentimiento <- ifelse(tokenizado$palabra %in% muy_positivo, "muy_positivo",
                                 ifelse(tokenizado$palabra %in% positivo, "positivo",
                                        ifelse(tokenizado$palabra %in% negativo, "negativo",
                                               ifelse(tokenizado$palabra %in% muy_negativo, "muy_negativo",
                                                      NA))))  # NA para palabras que no aportan

# Quitar palabras NA
tokenizado_filtrado <- tokenizado[!is.na(tokenizado$sentimiento), ]

# Tabla de sentimientos más limpia
tabla_sentimientos <- table(tokenizado_filtrado$sentimiento)
print(tabla_sentimientos)


tabla_sentimientos <- table(tokenizado$sentimiento)
print(tabla_sentimientos)

library(ggplot2)

# Convertir la tabla a data frame
df_sentimientos <- as.data.frame(tabla_sentimientos)
colnames(df_sentimientos) <- c("Sentimiento", "Cantidad")

# Graficar
ggplot(df_sentimientos, aes(x = Sentimiento, y = Cantidad, fill = Sentimiento)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("muy_positivo"="darkgreen",
                               "positivo"="lightblue",
                               "neutral"="grey",
                               "negativo"="orange",
                               "muy_negativo"="red")) +
  theme_minimal() +
  labs(title = "Distribución de Sentimientos",
       x = "Sentimiento",
       y = "Cantidad de Palabras")


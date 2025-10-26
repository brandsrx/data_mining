# ===============================================
# 1. CARGAR PAQUETES Y LIBRERÍAS
# ===============================================

# Instalar los paquetes necesarios (descomentar para ejecutar la primera vez)
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("tidyr") # Aunque no se carga directamente en la imagen 1, es útil para el enfoque tidy

# Cargar las librerías
library(tidytext)
library(dplyr)
library(tidyr) # Se incluye aunque la imagen solo muestre tidytext y dplyr

# ===============================================
# 2. CREAR Y CARGAR DATOS
# ===============================================

# Crear un marco de datos con el texto proporcionado, usando "doc" como identificador de documento
texto <- tibble(documento = c("doc1", "doc2", "doc3", "doc4", "doc5", "doc6", "doc7", "doc8", "doc9", "doc10"),
                contenido = c("La política en Bolivia pone triste a la gente.",
                              "El partido para el mundial una pena",
                              "Las marchas son el pan de cada día",
                              "Estan pensando subir el costo del pan",
                              "Los transportistas amenazan cada día",
                              "Los transportistas quieren entran en paro por el problema del gas",
                              "Estamos entrando en un frente frío",
                              "Cada día suben las cosas",
                              "El pollo otra vez ha subido",
                              "De todos modos subio el pasaje"))

# ===============================================
# 3. LIMPIEZA DE TEXTO (TOKENIZACIÓN Y STOP WORDS)
# ===============================================

# Tokenizar el texto (dividir en palabras individuales)
texto_palabras <- texto %>%
  unnest_tokens(word, contenido)

# Cargar las palabras vacías (stop words) para español
stop_words <- tidytext::get_stopwords(language = "es")

# Eliminar las palabras vacías utilizando anti_join
texto_limpio <- texto_palabras %>%
  anti_join(stop_words, by = "word")

# ===============================================
# 4. CÁLCULO DE TF-IDF
# ===============================================

# Contar la frecuencia de cada palabra en cada documento (Frecuencia de Término, TF)
frecuencia_palabras <- texto_limpio %>%
  count(documento, word, sort = TRUE)

# Calcular el TF-IDF, que asigna pesos a cada palabra
texto_tf_idf <- frecuencia_palabras %>%
  # La función bind_tf_idf toma: término, documento y la frecuencia (n)
  bind_tf_idf(term = word, document = documento, n = n)

# ===============================================
# 5. VISUALIZACIÓN DE RESULTADOS
# ===============================================

# Visualiza los pesos tf/idf de cada palabra
print(texto_tf_idf)

# Puedes ver las palabras más importantes (con mayor TF-IDF)
texto_tf_idf %>%
  arrange(desc(tf_idf))





library(tidytext)
library(dplyr)
library(ggplot2)
library(tm)
library(scales)

# ===============================================
# 1. TEXTO COMPLETO 🎶
# ===============================================
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

# Convertir a tibble
texto_tokens <- tibble(token = unlist(strsplit(tolower(texto), "\\s+")))

# ===============================================
# 2. LIMPIEZA DE TOKENS
# ===============================================
# Stopwords
stop_words_base <- tidytext::get_stopwords(language = "es")
stop_words_extra <- tm::stopwords("spanish")

stop_words <- unique(c(stop_words_base$word, stop_words_extra, 
                       "fuiste","tú","decir","insistir","hubo","culpable","fácil","vez","nada"))

texto_limpio <- texto_tokens %>%
  filter(!token %in% stop_words) %>%
  filter(grepl("[a-záéíóúñ]", token))  # eliminar símbolos o números

# ===============================================
# 3. TF-IDF ADAPTADO (frecuencia relativa)
# ===============================================
frecuencia <- texto_limpio %>%
  count(token, sort = TRUE) %>%
  mutate(tf_idf_adaptado = n / sum(n) * 100)  # porcentaje
print(frecuencia)
# ===============================================
# 4. VISUALIZACIÓN
# ===============================================
frecuencia %>%
  slice_max(tf_idf_adaptado, n = 15) %>%
  ggplot(aes(x = reorder(token, tf_idf_adaptado), y = tf_idf_adaptado, fill = tf_idf_adaptado)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(tf_idf_adaptado, 1)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Top 15 Tokens más importantes (TF-IDF adaptado)",
       x = "Token",
       y = "Frecuencia relativa (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16))



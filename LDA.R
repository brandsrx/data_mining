# ===============================================
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ===============================================

# Instalar los paquetes necesarios (descomentar para ejecutar la primera vez)
# install.packages("tidytext")
# install.packages("dplyr")
install.packages("topicmodels")
# install.packages("tidyr")
install.packages("reshape2")
# install.packages("ggplot2")

# Cargar las librerías
library(tidytext)
library(dplyr)
library(topicmodels)
library(tidyr)
library(reshape2)
library(ggplot2)

# ===============================================
# 2. PREPARACIÓN DE LOS DATOS Y LIMPIEZA
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

# Tokenizar el texto y eliminar palabras vacías (stop words)

# 1. Tokenización: dividir en palabras individuales
texto_palabras <- texto %>%
  unnest_tokens(word, contenido)

# Cargar las palabras vacías predefinidas del paquete tidytext (para español)
stop_words <- tidytext::get_stopwords(language = "es")

# 2. Limpieza: Eliminar las palabras vacías utilizando anti_join
texto_limpio <- texto_palabras %>%
  anti_join(stop_words, by = "word")

# Contar la frecuencia de cada palabra (opcional, para verificación)
frecuencia_palabras <- texto_limpio %>%
  count(word, sort = TRUE)

# ===============================================
# 3. CREACIÓN DE LA MATRIZ DE TÉRMINOS
# ===============================================

# Convertir el documento limpio a un DocumentTermMatrix (DTM)
# Primero se cuenta la frecuencia de las palabras limpias por documento (línea)
texto_dtm <- texto_limpio %>%
  count(linea, word) %>%
  
  # Uso de la función cast_dtm para crear la matriz de términos
  # La matriz tiene "linea" como documentos, "word" como términos y "n" como frecuencia
  tidytext::cast_dtm(linea, word, n)


# ===============================================
# 4. APLICACIÓN DEL ALGORITMO LDA
# ===============================================

# Aplicar el algoritmo LDA para identificar temas
# Se decide que k_topics = 4 # Número de temas a encontrar
k_topics <- 4

# Ejecutar el modelo LDA (Latent Dirichlet Allocation)
# Se usa el método "Gibbs" y se especifica un control para reproducibilidad (seed)
modelo_lda <- LDA(texto_dtm, 
                  k = k_topics, 
                  method = "Gibbs",
                  control = list(seed = 1:5, nstart = 5, verbose = 1000))

# ===============================================
# 5. EXTRACCIÓN Y ANÁLISIS DE RESULTADOS
# ===============================================

# Probabilidad de cada palabra en cada tema (beta)
prob_palabras_temas <- tidy(modelo_lda, matrix = "beta")

# Probabilidad de cada tema en cada documento (gamma)
prob_temas_documentos <- tidy(modelo_lda, matrix = "gamma")

# ===============================================
# 6. VISUALIZACIÓN DE LOS TEMAS
# ===============================================

# Visualiza los términos más relevantes (top 10) para cada tema
prob_palabras_temas %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  
  # Ordenar los términos por su probabilidad (beta) dentro de cada tema
  arrange(topic, -beta) %>%
  
  # Crear el gráfico de barras
  ggplot(aes(x = reorder(term, beta), y = beta)) +
  geom_col(show.legend = FALSE) +
  
  # Separar los gráficos por tema (topic)
  facet_wrap(~ topic, scales = "free_y") +
  
  # Voltear el sistema de coordenadas para un mejor formato
  coord_flip() +
  
  # Aplicar un tema visual minimalista
  theme_minimal()
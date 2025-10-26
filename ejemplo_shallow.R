# Instalar y cargar los paquetes necesarios
# install.packages("tidytext")
# install.packages("dplyr")
library(tidytext)
library(dplyr)

# Crear un marco de datos con el texto proporcionado
texto <- tibble(linea = 1:10,
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

# Dividir el texto en palabras individuales (tokenización)
texto_palabras <- texto %>%
  unnest_tokens(word, contenido)

# Ver las palabras antes de cualquier limpieza
print(texto_palabras)

# Cargar las palabras vacías predefinidas del paquete tidytext (para español)
stop_words <- tidytext::get_stopwords(language = "es")

# Eliminar las palabras vacías utilizando anti_join
texto_limpio <- texto_palabras %>%
  anti_join(stop_words, by = "word")

# Ver el resultado después de la limpieza
print(texto_limpio)

# Contar la frecuencia de cada palabra
frecuencia_palabras <- texto_limpio %>%
  count(word, sort = TRUE)

# Ver la frecuencia de las palabras
print(frecuencia_palabras)


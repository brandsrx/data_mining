library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(textclean)

#Cargando los archivos
df <- read.csv("datasets/messages_group.csv")
patron_url <- "^(https?|ftp)://[[:alnum:].-]+(?:/[[:alnum:]_.,!~*'()-]*)*$"


# Encontrar los enlaces mas enviados
datos_urls <- df %>%
  filter(           # Filtra las cadenas de texto vacías
    grepl(patron_url, text) # Filtra las URLs, usando el operador de negación (!)
  ) %>% select(text)
datos_urls <- datos_urls %>%
  mutate(domain = str_extract(text, "(?<=https://)[^/]+"))

domain_freq <- datos_urls %>% count(domain, sort = TRUE) %>%
  mutate(pct = n/sum(n) * 100) %>% 
  arrange(desc(pct))

print(domain_freq)


ggplot(domain_freq, aes(x = reorder(domain,n), y = n, fill = domain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs( title = "Frecuencia de enlaces enviados",
        x = "Dominio",
        y = "Porcentaje (%)") +
  theme(legend.position = "none")+
  geom_text(aes(label = paste0(round(pct,1), "%")), hjust = -0.1)

#filtrar datos vacios
df <- (df %>% filter(
  text != ""
))

df$text <- df$text %>%
  str_replace_all("http\\S+|ww\\S+","") %>%
  str_replace_all("[[:punct:]]","")


library(stringr)
library(stringi)

emojis_en_texto <- stri_extract_all_boundaries(
  df$text,
  type = "character"
)

# Filtrar solo los que son emojis
# Usamos regex que capture cualquier emoji (incluso recientes)
library(stringr)

# Regex avanzada para casi todos los emojis modernos
emoji_regex_avanzado <- paste0(
  "(",
  # Caras y emociones
  "[\U0001F600-\U0001F64F]|",
  # Símbolos y pictogramas
  "[\U0001F300-\U0001F5FF]|",
  # Transporte y mapas
  "[\U0001F680-\U0001F6FF]|",
  # Símbolos adicionales
  "[\U0001F700-\U0001F77F]|",
  "[\U0001F780-\U0001F7FF]|",
  "[\U0001F800-\U0001F8FF]|",
  "[\U0001F900-\U0001F9FF]|",
  "[\U0001FA00-\U0001FA6F]|",
  "[\U0001FA70-\U0001FAFF]|",
  "[\U0001FB00-\U0001FBFF]|",
  # Símbolos generales
  "[\U00002600-\U000026FF]|",
  "[\U00002700-\U000027BF]|",
  "\U0000FE0F|",                # Variante de presentación
  # Tonos de piel
  "[\U0001F3FB-\U0001F3FF]|",
  # Banderas
  "[\U0001F1E6-\U0001F1FF]|",
  # Zero Width Joiner para secuencias complejas
  "\u200D",
  ")+"
)

# Convertir a vector y filtrar solo emojis
emojis_vector <- unlist(emojis_en_texto)
emojis_vector <- emojis_vector[stri_detect_regex(emojis_vector, emoji_regex_avanzado)]

# Únicos y frecuencia
emojis_unicos <- sort(unique(emojis_vector))
frecuencia_emojis <- sort(table(emojis_vector), decreasing = TRUE)

# Diccionario de emojis → palabras en español
# Diccionario de emojis para análisis de sentimiento
diccionario_emoji <- c(
  # Positivos
  "☺️" = "cara sonriendo", "✅" = "marca de correcto", "✌🏼" = "mano de victoria",
  "✨" = "brillo", "✨️" = "brillo", "❤️" = "corazon rojo", "❤️‍🔥" = "corazon en llamas",
  "❤️‍🩹" = "corazon vendado", "🌟" = "estrella brillante", "🌸" = "flor de cerezo",
  "🍀" = "trebol de la suerte", "🍫" = "chocolate", "🍬" = "caramelo",
  "🎁" = "caja de regalo", "🎇" = "fuegos artificiales", "🎈" = "globo",
  "🎉" = "confeti de fiesta", "🎊" = "confeti de celebracion", "🏆" = "trofeo",
  "👍" = "pulgar arriba", "👍🏻" = "pulgar arriba piel clara", "👏" = "aplausos",
  "👏🏻" = "aplausos piel clara", "👏🏽" = "aplausos piel morena", "💪" = "brazo musculoso",
  "💫" = "estrellas de energia", "🔥" = "fuego", "💙" = "corazon azul",
  "💜" = "corazon morado", "🖤" = "corazon negro", "🤑" = "cara con billetes",
  "🤩" = "cara con estrellas en los ojos", "🥲" = "cara sonriendo con lagrima",
  "🥳" = "cara de fiesta", "🥳️" = "cara de fiesta", "🥹" = "cara de ternura suplicante",
  "🫂" = "personas abrazandose", "🫠" = "cara derritiendose", "🫡" = "cara saludando militarmente",
  "😀" = "cara sonriendo", "😁" = "cara sonriendo con ojos grandes", "😂" = "cara llorando de risa",
  "😃" = "cara sonriente grande", "😄" = "cara sonriente con ojos de felicidad",
  "😅" = "cara sonriendo con sudor", "😇" = "cara de angel", "😉" = "cara guiñando el ojo",
  "😉️️️" = "cara guiñando el ojo", "😊" = "cara sonriendo con mejillas rosadas", "😌" = "cara aliviada",
  "😎" = "cara con gafas de sol", "😜" = "cara guiñando con lengua fuera",
  "😝" = "cara con lengua fuera", "🙂" = "cara con sonrisa sutil", "🙂‍↕️" = "cara con sonrisa sutil",
  "🙃" = "cara boca abajo", "🙌" = "manos levantadas celebrando",
  "🙌🏻" = "manos levantadas piel clara", "🙌🏼" = "manos levantadas piel morena",
  "🙏" = "manos rezando", "🙏🏻" = "manos rezando piel clara", "🙏🏼" = "manos rezando piel morena",
  "🚀" = "cohete", "🟢" = "circulo verde", "💃" = "mujer bailando",
  "🕺" = "hombre bailando", "🤙" = "mano llamame", "🤝" = "apreton de manos",
  "🤠" = "cara de vaquero", "🤣" = "cara riendose a carcajadas",
  "🥺" = "cara suplicante", "🦋" = "mariposa",
  
  # Negativos
  "☠" = "calavera", "☠️" = "calavera", "⚠" = "senal de advertencia", "⚠️" = "senal de advertencia",
  "❎" = "marca de incorrecto", "❗" = "signo de exclamacion", "💔" = "corazon roto",
  "👹" = "duende japones", "👺" = "ogro japones", "👻" = "fantasma", "💀" = "calavera",
  "💩" = "pila de caca", "🏴‍☠️" = "bandera pirata", "🐉" = "dragon",
  "🐍" = "serpiente", "🤬" = "cara con simbolos de insultos",
  "🤭" = "cara con la mano sobre la boca", "😡" = "cara de enojo",
  "😢" = "cara llorando", "😣" = "cara de perseverancia", "😥" = "cara triste con sudor",
  "😦" = "cara con la boca abierta", "😩" = "cara de dolor agotada", "😪" = "cara de sueno",
  "😫" = "cara de agotamiento", "😬" = "cara de disgusto",
  "😭" = "cara llorando a gritos", "😱" = "cara de miedo", "😲" = "cara de asombro",
  "😳" = "cara avergonzada", "😵‍💫" = "cara con espirales en los ojos",
  "😿" = "gato llorando", "😒" = "cara de desaprobacion", "😓" = "cara de alivio con sudor",
  "😔" = "cara pensativa triste", "😕" = "cara confundida", "😖" = "cara de sufrimiento",
  "😞" = "cara de decepcion", "🤧" = "cara estornudando", "🤨" = "cara con ceja levantada",
  "🤡" = "cara de payaso", "🫢" = "cara jadeando", "🫣" = "cara con los ojos cubiertos",
  "🫥" = "cara sin boca", "🫦" = "labio mordido",
  
  # Neutros / Objetos / Lugares
  "⏱️" = "cronometro", "☁️️" = "nube", "☕" = "taza de cafe", "☝️" = "dedo indice arriba",
  "♦" = "diamante", "⚔️" = "espadas cruzadas", "⚙️" = "engranaje",
  "⚡" = "rayo", "⚽" = "balon de futbol", "⛽" = "bomba de gasolina", "🇧🇴" = "bandera de Bolivia",
  "✈" = "avion", "✋🏼" = "mano levantada piel morena", "✍️" = "mano escribiendo",
  "✏️" = "lapiz", "✳️" = "asterisco", "❓" = "signo de interrogacion",
  "➡️" = "flecha a la derecha", "⬇️" = "flecha hacia abajo", "🌌" = "cielo nocturno galactico",
  "🌍" = "planeta tierra europa africa", "🌎" = "planeta tierra americas",
  "🌐" = "globo terraqueo", "🌚" = "luna nueva", "🌡" = "termometro", "🎃" = "calabaza",
  "🎓" = "birrete de graduacion", "🎙️" = "microfono de estudio", "🎞️" = "carrete de pelicula",
  "🎟" = "boleto", "🎟️" = "boleto", "🎤" = "microfono", "🎥" = "camara de cine",
  "🎨" = "paleta de artista", "🎫" = "entrada de evento", "🎮" = "control de videojuego",
  "🎯" = "diana de tiro", "🎲" = "dados", "🏀" = "balon de baloncesto", "🏐" = "balon de voleibol",
  "🏔" = "montana", "🏢" = "edificio de oficinas", "🏫" = "escuela", "🐙" = "pulpo",
  "🐧" = "pinguino", "🐯" = "cara de tigre", "👀" = "ojos", "👁️‍🗨️" = "ojo en burbuja de dialogo",
  "👆" = "dedo indice arriba", "👆🏽" = "dedo indice arriba piel morena", "👇" = "dedo indice abajo",
  "👇🏻" = "dedo indice abajo piel clara", "👈" = "dedo indice izquierda",
  "👉" = "dedo indice derecha", "👉🏻" = "dedo indice derecha piel clara",
  "👉🏼" = "dedo indice derecha piel morena", "👊" = "puño", "👋" = "mano saludando",
  "👋🏻" = "mano saludando piel clara", "👋🏼" = "mano saludando piel morena",
  "👌🏻" = "mano ok piel clara", "👍🏻" = "pulgar arriba piel clara", "👥" = "siluetas de personas",
  "👨‍🎓" = "hombre graduado", "👨‍🏫" = "hombre profesor", "👨🏻‍💻" = "hombre programador piel clara",
  "👨🏽‍🎓" = "hombre graduado piel morena", "👨‍👩‍👧‍👦" = "familia", "👨‍💻" = "hombre programador",
  "👩‍🎓" = "mujer graduada", "👩‍🏫" = "mujer profesora", "👩🏻‍🎓" = "mujer graduada piel clara",
  "👩🏻‍💻" = "mujer programadora piel clara", "👩‍💻" = "mujer programadora", "👸" = "princesa",
  "💡" = "bombilla", "💬" = "burbuja de dialogo", "💰" = "bolsa de dinero", "💵" = "billete de dolar",
  "💸" = "dinero volando", "💻" = "computadora portatil", "💼" = "maletin", "📂" = "carpeta abierta",
  "📄" = "hoja de papel", "📅" = "calendario", "📆" = "calendario de pared", "📈" = "grafico de tendencia",
  "📊" = "grafico de barras", "📋" = "portapapeles", "📌" = "chincheta", "📍" = "marcador de mapa",
  "📎" = "clip de papel", "📕" = "libro rojo", "📖" = "libro abierto", "📗" = "libro verde",
  "📘" = "libro azul", "📚" = "libros", "📜" = "pergamino", "📝" = "nota", "📞" = "telefono fijo",
  "📡" = "antena satelite", "📢" = "megafono", "📣" = "megafono de mano", "📧" = "correo electronico",
  "📩" = "sobre de correo", "📱" = "telefono movil", "📲" = "telefono movil con flecha",
  "📸" = "camara de fotos", "🔄" = "flechas de recarga", "🔊" = "altavoz", "🔌" = "enchufe",
  "🔍" = "lupa de busqueda", "🔐" = "candado con llave", "🔑" = "llave", "🔒" = "candado cerrado",
  "🔓" = "candado abierto", "🔔" = "campana", "🔗" = "eslabon de cadena", "🔧" = "llave inglesa",
  "🔬" = "microscopio", "🔮" = "bola de cristal", "🔴" = "circulo rojo", "🔵" = "circulo azul",
  "🔸" = "diamante naranja", "🔹" = "diamante azul", "🕐" = "reloj una en punto",
  "🕒" = "reloj tres en punto", "🕓" = "reloj cuatro en punto", "🕔" = "reloj cinco en punto",
  "🕕" = "reloj seis en punto", "🕖" = "reloj siete en punto", "🕗" = "reloj ocho en punto",
  "🕘" = "reloj nueve en punto", "🕙" = "reloj diez en punto", "🕵️‍♀️" = "mujer detective",
  "🕵️‍♂️" = "hombre detective", "🕸️" = "telarana", "🖊" = "pluma", "🖖🏻" = "saludo vulcano",
  "🖥" = "monitor de computadora", "🖥️" = "monitor de computadora", "🖱️" = "raton de computadora",
  "🖼" = "cuadro enmarcado", "🗓️" = "calendario de escritorio", "🗓" = "calendario de escritorio",
  "🗳️" = "urna electoral", "🗿" = "estatua moai", "🤌" = "mano de pizzaiolo",
  "🤓" = "cara de nerd", "🤔" = "cara pensativa", "🤖" = "cara de robot", "🤗" = "cara abrazando",
  "🤷" = "persona encogiendose de hombros", "🤷‍♂" = "hombre encogiendose de hombros",
  "🤷🏻" = "persona encogiendose de hombros piel clara", "🤷🏻‍♂️" = "hombre encogiendose de hombros piel clara",
  "🧐" = "cara con monoculo", "🧑‍🏫" = "persona profesora", "🧑‍💻" = "persona programadora",
  "🧑‍🦽‍➡️" = "persona en silla de ruedas", "🧠" = "cerebro", "🧩" = "pieza de rompecabezas",
  "🫅" = "persona con corona", "😮‍💨" = "cara exhalando", "😴" = "cara dormida",
  "🙄" = "cara con ojos en blanco", "🙆‍♂" = "hombre con los brazos por encima de la cabeza",
  "🙈" = "mono cubriendose los ojos", "🙋🏻‍♂️" = "hombre levantando la mano piel clara",
  "🚩" = "bandera triangular", "🛍️" = "bolsa de compras", "🛑" = "senal de stop",
  "🛠" = "martillo y llave inglesa", "🛡" = "escudo", "🟠" = "circulo naranja",
  "🟡" = "circulo amarillo", "1️⃣" = "numero uno", "2️⃣" = "numero dos", "Y️" = "letra y",
  "\U0001fae9" = "cara con la boca en diagonal",
  "👌" = "mano que indica 'todo en orden' o 'de acuerdo'",
  "🤫" = "cara con el dedo sobre la boca indicando silencio o un secreto",
  "🥷" = "ninja o figura sigilosa",
  "🫵🏻" = "dedo indice apuntando hacia la persona",
  "😈" = "cara de diablillo sonriendo, a menudo con connotación de travesura o malicia",
  "😏" = "cara con sonrisa de satisfacción o connotación de coquetería o picardía",
  "😐" = "cara neutral sin emoción",
  "😑" = "cara de disgusto o 'pasando' de algo"
)


emojis_faltantes <- setdiff(emojis_unicos, names(diccionario_emoji))

# Ver resultados
length(emojis_faltantes)

reemplazar_emojis_fijo <- function(textos, dic) {
  for(e in names(dic)) {
    # Agregar espacio antes y después del reemplazo
    textos <- stri_replace_all_fixed(textos, e, paste0(" ", dic[[e]], " "), vectorize_all = FALSE)
  }
  # Quitar espacios al inicio y final y reducir múltiples espacios a uno solo
  textos <- stri_trim_both(textos)
  textos <- stri_replace_all_regex(textos, "\\s+", " ")
  return(textos)
}

df$text <- sapply(df$text, reemplazar_emojis_fijo,dic = diccionario_emoji)

# Ejemplo: limpieza básica
mensajes <- df %>%
  mutate(text_clean = str_to_lower(text),
         text_clean = str_replace_all(text_clean, "http\\S+|www\\S+", ""),   # URLs
         text_clean = str_replace_all(text_clean, "@\\w+", ""),              # menciones
         text_clean = str_replace_all(text_clean, "[^\\w\\s]", ""),          # puntuación
         text_clean = str_replace_all(text_clean, "x[0-9]+","yo tambi
                                      en"),
         text_clean = str_replace_all(text_clean, "[0-9]+", ""),
         text_clean = str_replace_all(text_clean, "ok","esta bien"))  %>%            # números
         filter( text_clean != "")

palabras_cortas <- mensajes %>%
  filter(str_length(text_clean) <= 2) %>%  # palabras de 1 o 2 letras
  count(text_clean, sort = TRUE)

palabras_cortas <- mensajes %>%
  filter(str_squish(text_clean) == "a")   # palabras de 1 o 2 letras
  
print(palabras_cortas)

dicc_cortas <- tibble::tibble(
  original = c( "si","se", "sí", "no", "xd", "ya", "v", "gg", "hi","zi","zy"),
  reemplazo = c( "afirmacion","afirmacion", "afirmacion", "negacion", "risa",
                "afirmacion", "afirmacion", "risa", "saludo","afirmacion","afirmacion")
)

mensajes <- mensajes %>%
  left_join(dicc_cortas, by = c("text_clean" = "original")) %>%
  mutate(text_clean = ifelse(!is.na(reemplazo), reemplazo, text_clean)) %>%
  filter(nchar(str_squish(text_clean)) > 2) %>%
  select(-reemplazo)

# tokenizar
library(dplyr)
library(tidytext)
library(stringr)
library(stopwords)

stop_words <- data.frame(word = stopwords::stopwords("es"))

tokens_clean <- mensajes %>%
  unnest_tokens(word, text_clean) %>%        # tokenizar
  anti_join(stop_words, by = "word") %>%     # eliminar stopwords
  mutate(word = tolower(word),               # pasar a minúsculas
         word = gsub("[^a-záéíóúñ0-9 ]", " ", word), # símbolos a espacio
         word = str_squish(word)) %>%        # eliminar espacios extra
  filter(nchar(word) > 1)                    # eliminar tokens muy cortos

# =======================================
# LIBRERÍAS
# =======================================
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)
library(stopwords)

# =======================================
# TOKENIZACIÓN Y LIMPIEZA
# =======================================
stop_words <- data.frame(word = stopwords::stopwords("es"))

tokens_clean <- mensajes %>%
  unnest_tokens(word, text_clean) %>%           # tokenizar
  anti_join(stop_words, by = "word") %>%        # eliminar stopwords
  mutate(word = tolower(word),                  # minúsculas
         word = gsub("[^a-záéíóúñ0-9 ]", " ", word), # reemplaza símbolos
         word = str_squish(word)) %>%           # eliminar espacios extra
  filter(nchar(word) > 1)                       # eliminar tokens muy cortos

# =======================================
# TOP 10 PALABRAS MÁS FRECUENTES
# =======================================
top_words <- tokens_clean %>%
  count(word, sort = TRUE) %>%
  mutate(percent = n / sum(n)) %>%  # proporción 0-1
  slice_head(n = 10)

ggplot(top_words, aes(x = reorder(word, percent), y = percent)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(percent, accuracy = 0.1)), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Top 10 palabras más frecuentes",
    x = "Palabra",
    y = "Porcentaje (%)"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 14)

# =======================================
# ANÁLISIS DE EMOCIONES CON LEXICON NRC
# =======================================

emociones_basicas <- c("anger", "anticipation", "disgust", "fear", 
                       "joy", "sadness", "surprise", "trust")


tokens_nrc <- tokens_clean %>%
  inner_join(get_sentiments("nrc"), by = c("word" = "word")) %>%
  filter(sentiment %in% emociones_basicas)

frecuencia_sentimientos <- tokens_nrc %>%
  count(sentiment, sort = TRUE) %>%
  mutate(porcentaje = n / sum(n) * 100)

df_final <- frecuencia_sentimientos %>%
  mutate(
    emocion_es = case_when(
      sentiment == "anger"        ~ "Ira",
      sentiment == "anticipation"~ "Anticipación",
      sentiment == "disgust"      ~ "Disgusto",
      sentiment == "fear"         ~ "Miedo",
      sentiment == "joy"          ~ "Alegría",
      sentiment == "sadness"      ~ "Tristeza",
      sentiment == "surprise"     ~ "Sorpresa",
      sentiment == "trust"        ~ "Confianza",
      TRUE                        ~ sentiment
    ),
    emocion_es = fct_reorder(emocion_es, n)
  )

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

ggplot(df_final, aes(x = emocion_es, y = porcentaje, fill = emocion_es)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            hjust = -0.1, size = 4, fontface = "bold", color = "#2C3E50") +
  scale_fill_manual(values = colores_emociones) +
  coord_flip() +
  labs(
    title = "Análisis de Emociones mensajes de Telegram",
    subtitle = "Grupo: Curso de Temporada UMSA Informática",
    x = NULL,
    y = "Porcentaje (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, color = "#7F8C8D", margin = margin(b = 15)),
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






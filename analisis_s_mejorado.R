# ============================================
# ANÁLISIS DE SENTIMIENTOS MEJORADO EN R
# Comparación: Con y Sin Limpieza de Texto
# ============================================

# Cargar paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidytext, stopwords, scales, gridExtra, 
               syuzhet, wordcloud, RColorBrewer, viridis, patchwork)

# ============================================
# FUNCIÓN DE LIMPIEZA AVANZADA
# ============================================

limpiar_texto <- function(texto_df, idioma = "es") {
  
  # Tokenizar
  tokens <- texto_df %>%
    unnest_tokens(word, texto)
  
  # Stopwords según idioma
  stop_words_lang <- tibble(word = stopwords(idioma))
  
  # Limpieza avanzada
  texto_limpio <- tokens %>%
    # Convertir a minúsculas (ya está con unnest_tokens)
    # Eliminar stopwords
    anti_join(stop_words_lang, by = "word") %>%
    # Eliminar números
    filter(!str_detect(word, "^[0-9]+$")) %>%
    # Eliminar palabras muy cortas (menos de 3 caracteres)
    filter(str_length(word) >= 3) %>%
    # Eliminar caracteres especiales residuales
    filter(str_detect(word, "^[a-záéíóúñü]+$")) %>%
    # Eliminar palabras que son solo vocales repetidas
    filter(!str_detect(word, "^[aeiouáéíóú]+$"))
  
  return(list(
    original = tokens,
    limpio = texto_limpio
  ))
}

# ============================================
# EJEMPLO 1: TEXTO EN ESPAÑOL
# ============================================

texto_es <- "Fuiste tú
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

df_es <- tibble(line = 1, texto = texto_es)

# Aplicar limpieza
resultados_es <- limpiar_texto(df_es, idioma = "es")

cat("=== ANÁLISIS TEXTO EN ESPAÑOL ===\n")
cat("Palabras originales:", nrow(resultados_es$original), "\n")
cat("Palabras después de limpieza:", nrow(resultados_es$limpio), "\n")
cat("Reducción:", round((1 - nrow(resultados_es$limpio)/nrow(resultados_es$original))*100, 1), "%\n\n")

# ============================================
# FRECUENCIA DE PALABRAS: COMPARACIÓN
# ============================================

# Frecuencia ORIGINAL
freq_original_es <- resultados_es$original %>%
  count(word, sort = TRUE) %>%
  mutate(porcentaje = round(100 * n / sum(n), 2),
         tipo = "Original") %>%
  slice_head(n = 15)

# Frecuencia LIMPIA
freq_limpia_es <- resultados_es$limpio %>%
  count(word, sort = TRUE) %>%
  mutate(porcentaje = round(100 * n / sum(n), 2),
         tipo = "Limpio") %>%
  slice_head(n = 15)

# Visualización comparativa
p1_es <- ggplot(freq_original_es, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")), 
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15") +
  coord_flip() +
  labs(title = "Texto Original (con stopwords)",
       x = NULL, y = "Frecuencia") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.15)))

p2_es <- ggplot(freq_limpia_es, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")), 
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#08519c") +
  coord_flip() +
  labs(title = "Texto Limpio (sin stopwords)",
       x = NULL, y = "Frecuencia") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.15)))

# Combinar gráficos
grid.arrange(p1_es, p2_es, ncol = 2,
             top = "COMPARACIÓN: Frecuencia de Palabras - Texto Español")

# ============================================
# ANÁLISIS DE SENTIMIENTOS: COMPARACIÓN
# ============================================

# Sentimientos ORIGINAL
sent_original_es <- get_nrc_sentiment(resultados_es$original$word, language = "spanish")
freq_sent_original_es <- tibble(
  emocion = names(colSums(sent_original_es[, 1:8])),
  frecuencia = as.numeric(colSums(sent_original_es[, 1:8])),
  tipo = "Original"
) %>%
  mutate(porcentaje = round(frecuencia / sum(frecuencia) * 100, 1))

# Sentimientos LIMPIO
sent_limpio_es <- get_nrc_sentiment(resultados_es$limpio$word, language = "spanish")
freq_sent_limpio_es <- tibble(
  emocion = names(colSums(sent_limpio_es[, 1:8])),
  frecuencia = as.numeric(colSums(sent_limpio_es[, 1:8])),
  tipo = "Limpio"
) %>%
  mutate(porcentaje = round(frecuencia / sum(frecuencia) * 100, 1))

# Combinar ambos
comparacion_sent_es <- bind_rows(freq_sent_original_es, freq_sent_limpio_es)

# Traducir emociones
comparacion_sent_es <- comparacion_sent_es %>%
  mutate(emocion_es = case_when(
    emocion == "anger" ~ "Ira",
    emocion == "anticipation" ~ "Anticipación",
    emocion == "disgust" ~ "Disgusto",
    emocion == "fear" ~ "Miedo",
    emocion == "joy" ~ "Alegría",
    emocion == "sadness" ~ "Tristeza",
    emocion == "surprise" ~ "Sorpresa",
    emocion == "trust" ~ "Confianza"
  ))

# Visualización comparativa de sentimientos
ggplot(comparacion_sent_es, aes(x = reorder(emocion_es, frecuencia), 
                                y = frecuencia, fill = tipo)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(porcentaje, "%")), 
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Original" = "#fc8d59", "Limpio" = "#91bfdb"),
                    name = "Tipo de Texto") +
  coord_flip() +
  labs(title = "COMPARACIÓN: Análisis de Emociones",
       subtitle = "Impacto de la limpieza de texto en la detección de sentimientos",
       x = NULL, y = "Frecuencia de palabras") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
        legend.position = "top",
        panel.grid.major.y = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.2)))

# ============================================
# SENTIMIENTO GENERAL: POSITIVO VS NEGATIVO
# ============================================

sent_general_comparacion <- tibble(
  tipo_texto = rep(c("Original", "Limpio"), each = 2),
  sentimiento = rep(c("Positivo", "Negativo"), 2),
  valor = c(
    sum(sent_original_es$positive), sum(sent_original_es$negative),
    sum(sent_limpio_es$positive), sum(sent_limpio_es$negative)
  )
) %>%
  group_by(tipo_texto) %>%
  mutate(porcentaje = round(valor / sum(valor) * 100, 1)) %>%
  ungroup()

ggplot(sent_general_comparacion, aes(x = tipo_texto, y = porcentaje, fill = sentimiento)) +
  geom_col(position = "fill", width = 0.6) +
  geom_text(aes(label = paste0(porcentaje, "%\n(", valor, ")")),
            position = position_fill(vjust = 0.5),
            size = 4.5, fontface = "bold", color = "white") +
  scale_fill_manual(values = c("Positivo" = "#2ecc71", "Negativo" = "#e74c3c"),
                    name = "Sentimiento") +
  scale_y_continuous(labels = percent) +
  labs(title = "Sentimiento General: Original vs Limpio",
       subtitle = "Balance emocional del texto",
       x = NULL, y = "Proporción") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        legend.position = "top",
        panel.grid.major.x = element_blank())

# ============================================
# NUBE DE PALABRAS (SOLO TEXTO LIMPIO)
# ============================================

set.seed(123)
freq_para_nube <- resultados_es$limpio %>%
  count(word, sort = TRUE)

wordcloud(words = freq_para_nube$word, 
          freq = freq_para_nube$n,
          min.freq = 1,
          max.words = 50,
          random.order = FALSE,
          rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
title(main = "Nube de Palabras - Texto Limpio", 
      font.main = 2, cex.main = 1.5)

# ============================================
# RESUMEN ESTADÍSTICO
# ============================================

cat("\n=== RESUMEN COMPARATIVO ===\n\n")

cat("TEXTO ORIGINAL:\n")
cat("Total de palabras:", nrow(resultados_es$original), "\n")
cat("Palabras únicas:", n_distinct(resultados_es$original$word), "\n")
cat("Sentimiento positivo:", sum(sent_original_es$positive), "\n")
cat("Sentimiento negativo:", sum(sent_original_es$negative), "\n\n")

cat("TEXTO LIMPIO:\n")
cat("Total de palabras:", nrow(resultados_es$limpio), "\n")
cat("Palabras únicas:", n_distinct(resultados_es$limpio$word), "\n")
cat("Sentimiento positivo:", sum(sent_limpio_es$positive), "\n")
cat("Sentimiento negativo:", sum(sent_limpio_es$negative), "\n\n")

cat("IMPACTO DE LA LIMPIEZA:\n")
cat("Reducción de palabras:", 
    round((1 - nrow(resultados_es$limpio)/nrow(resultados_es$original))*100, 1), "%\n")
cat("Cambio en sentimiento positivo:", 
    sum(sent_limpio_es$positive) - sum(sent_original_es$positive), "\n")
cat("Cambio en sentimiento negativo:", 
    sum(sent_limpio_es$negative) - sum(sent_original_es$negative), "\n")

# ============================================
# EXPORTAR RESULTADOS
# ============================================

# Crear carpeta de resultados
if (!dir.exists("resultados_analisis")) dir.create("resultados_analisis")

# Guardar datos
write_csv(freq_limpia_es, "resultados_analisis/frecuencia_palabras_limpio.csv")
write_csv(comparacion_sent_es, "resultados_analisis/comparacion_sentimientos.csv")
write_csv(sent_general_comparacion, "resultados_analisis/sentimiento_general.csv")

cat("\n✓ Análisis completado exitosamente\n")
cat("✓ Resultados guardados en 'resultados_analisis/'\n")
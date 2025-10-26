library(rvest)
library(dplyr)
library(ggplot2)

# Función para extraer productos de una página dada
# la primera pagina es ...
# las otras paginas tienen una numeracion (pagina_num)

# esta funcion toma el num de pag y construye la url correcta
extraer_productos <- function(pagina_num) {
  if (pagina_num == 1) {
    url <- "https://webscraper.io/test-sites/e-commerce/static/computers/laptops"
  } else {
    url <- paste0("https://webscraper.io/test-sites/e-commerce/static/computers/laptops?page=", pagina_num)
  }
  
  webpage <- read_html(url)
  
  titles <- webpage %>% html_nodes(".title") %>% html_attr("title")
  prices <- webpage %>% html_nodes(".price") %>% html_text()
  prices_num <- as.numeric(gsub("\\$", "", prices))
  
  data.frame(titulo = titles, precio = prices_num, stringsAsFactors = FALSE)
}

# Número total de páginas (en este caso sabemos que son 10 o mas revisar)
total_paginas <- 10

# Extraer datos de todas las páginas y unir con lapply (itera sobre todas las pag)
lista_productos <- lapply(1:total_paginas, extraer_productos)
productos_todos <- bind_rows(lista_productos) #une todos los df en uno solo

# Mostrar resultados
print(productos_todos)

# visualizacion

# Histograma de precios
ggplot(productos_todos, aes(x = precio)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribución de precios de laptops (todas las páginas)",
       x = "Precio (USD)", y = "Cantidad de productos") +
  theme_minimal()

# Top 5 productos más caros
productos_top <- productos_todos %>% arrange(desc(precio)) %>% head(5)

ggplot(productos_top, aes(x = reorder(titulo, precio), y = precio, fill = precio)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 5 laptops más caras",
       x = "Producto", y = "Precio (USD)") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  theme_minimal()

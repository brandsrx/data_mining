install.packages(c("rvest", "ggplot2", "dplyr"))
library(rvest)
library(ggplot2)
library(dplyr)

# URL de prueba para scraping
url <- "https://webscraper.io/test-sites/e-commerce/static/computers/laptops"

# Leer la página
webpage <- read_html(url)

# Extraer títulos de productos
titles <- webpage %>% html_nodes(".title") %>% html_attr("title")

# Extraer precios
prices <- webpage %>% html_nodes(".price") %>% html_text()

# Limpiar precios: quitar símbolo $ y convertir a numérico
prices_num <- as.numeric(gsub("\\$", "", prices))

# Crear dataframe
productos <- data.frame(titulo = titles, precio = prices_num)

# Mostrar primeros productos
head(productos)


ggplot(productos, aes(x = precio)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribución de precios de laptops",
       x = "Precio (USD)", y = "Cantidad de productos") +
  theme_minimal()

productos_top <- productos %>% arrange(desc(precio)) %>% head(5)

ggplot(productos_top, aes(x = reorder(titulo, precio), y = precio, fill = precio)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 5 laptops más caras",
       x = "Producto", y = "Precio (USD)") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  theme_minimal()

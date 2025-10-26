
install.packages(c("igraph", "ggraph", "tidygraph"))
library(igraph)
library(ggraph)
library(tidygraph)


pag1<- "https://www.octoparse.es/blog/los-10-sitios-web-m%C3%A1s-escrapeados"
pag2 <- "https://www.octoparse.es/blog/5-mejores-rastreadores-web-de-redes-sociales"

pag1 <- "https://www.octoparse.es/blog/los-10-sitios-web-m%C3%A1s-escrapeados"
pag3 <-"https://www.octoparse.es/blog/c%C3%B3mo-extraer-detalles-de-productos-de-mercado-libre"

pag1 <- "https://www.octoparse.es/blog/los-10-sitios-web-m%C3%A1s-escrapeados"
pag4 <- "https://www.octoparse.es/blog/c%C3%B3mo-raspar-anuncios-de-empleo-de-indeed"


# Definir enlaces: cada fila es un enlace de origen a destino
# se crea un df con enlaces entre nodos a paginas
enlaces <- data.frame(
  origen = c(pag1, pag1, pag1),
  destino = c(pag2, pag3, pag4)
)

# Se construye un grafo dirigido con igraph
grafo <- graph_from_data_frame(enlaces, directed = TRUE)

# se calcula métricas básicas como grado de entrada (links recibidos) y salida (links enviados).
grado_entrada <- degree(grafo, mode = "in")
grado_salida <- degree(grafo, mode = "out")

print(grado_entrada)
print(grado_salida)

# se visualiza la red con ggraph, mostrando nodos y flechas que indican dirección de enlaces.
ggraph(grafo, layout = "fr") +  # layout Fruchterman-Reingold
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm'), edge_colour = "gray") +
  geom_node_point(size = 5, color = "steelblue") +
  geom_node_text(aes(label = name), vjust = 1.8, size = 5) +
  theme_void() +
  ggtitle("Análisis sencillo de enlaces entre páginas")

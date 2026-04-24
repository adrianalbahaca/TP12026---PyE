# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("sf")
# install.packages("rnaturalearthdata")
# install.packages("rnaturalearth")
# install.packages("leaflet")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)


####################
# Mapas con ggplot #
####################

# ggplot crea mapas estáticos, como los gráficos, "imprimibles"

# Mapa del mundo
mundo <- ne_countries(scale = "medium", returnclass = "sf")

# Tus datos (ejemplo)
df <- data.frame(
  pais = c("Argentina", "Brasil", "Chile", "India", "Marruecos"),
  cod = c("ARG", "BRA", "CHL", "IND", "MAR"),
  index = c(0.7, 0.5, 0.8, 0.2, 0.9)
)

# Unir datos al mapa
mapa_datos <- mundo %>%
  left_join(df, by = c("iso_a3" = "cod"))

# Graficar
ggplot(mapa_datos) +
  geom_sf(aes(fill = index)) +
  scale_fill_viridis_c(na.value = "grey97") +
  theme_minimal() +
  labs(fill = "Índice")


####################
# Mapas con leaflet #
####################

# leaflet crea mapas dinámicos a través de objetos html

leaflet() %>%
  addTiles() # Así abrimos un mapa base

# Configuramos una paleta de colores para el mapa
pal <- colorNumeric("viridis", # Llamamos a una paleta ya existente
                    domain = mapa_datos$index, # Le asignamos valores
                    na.color = "transparent") # ¿Qué hacer para valores NA?

# Mapa interactivo
leaflet(mapa_datos) %>% 
  addTiles() %>% # Mapa base
  
  addPolygons( # Agregamos los polígonos de nuestros países
    fillColor = ~pal(index), # Coloreados según index
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~ paste(name, ": ", index) # Agregamos una etiqueta on hover
  ) %>%
  addLegend( # Configuramos la leyenda
    pal = pal, # con la misma paleta de colores que usamos en el mapa
    values = ~ index, # y los mismos valores
    title = "Índice"
  )

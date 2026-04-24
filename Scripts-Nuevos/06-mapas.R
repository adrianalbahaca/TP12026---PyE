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

# -------------------------------------------------------------------------
# Mapa GIRAI
mundo <- ne_countries(scale="medium", returnclass="sf")

mundo %>%
  dplyr::left_join(datos_limpios, by = c("iso_a3" = "ISO3")) %>%
  dplyr::mutate(
    GIRAI_intervalo = cut(GIRAI,
                          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                          labels = c("0-10", "10-20", "20-30", "30-40", "40-50",
                                     "50-60", "60-70", "70-80", "80-90", "90-100"),
                          include.lowest = TRUE)
  ) %>%
  ggplot() +
  geom_sf(aes(fill = GIRAI_intervalo)) +
  scale_fill_brewer(
    palette  = "RdYlGn",
    na.value = "grey80",
    name     = "GIRAI"
  ) +
  labs(
    title   = "Índice Global de IA Responsable (GIRAI) por país",
    caption = "Fuente: GIRAI 2024"
  ) +
  theme_minimal()

ggsave("mapa_girai.png", width = 12, height = 6, dpi = 300)

mundo %>%
  dplyr::left_join(datos_limpios, by = c("iso_a3" = "ISO3")) %>%
  ggplot() +
  geom_sf(aes(fill = lider)) +
  scale_fill_manual(
    values   = c("Líder" = "steelblue", "Resto" = "tomato"),
    na.value = "grey80",
    name     = "Grupo"
  ) +
  labs(
    title   = "Países líderes en IA Responsable (GIRAI > 40)",
    caption = "Fuente: GIRAI 2024"
  ) +
  theme_minimal()
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

# -----------------------------------------------------------------------
# Mapa del p70

datos_limpios <- datos_limpios %>%
  dplyr::mutate(
    total_p70 = p70_sesgo + p70_infancia + p70_divers + p70_datpers +
      p70_genero + p70_suphum + p70_laboral + p70_segu + p70_transp
  )

mundo %>%
  dplyr::left_join(datos_limpios, by = c("iso_a3" = "ISO3")) %>%
  dplyr::mutate(
    supera_alguna = factor(ifelse(total_p70 > 0, "Sí", "No"))
  ) %>%
  ggplot() +
  geom_sf(aes(fill = supera_alguna)) +
  scale_fill_manual(
    values   = c("Sí" = "steelblue", "No" = "tomato"),
    na.value = "grey80",
    name     = "Supera 70\nen algún área"
  ) +
  labs(
    title   = "Países que superan 70 puntos en al menos un área temática",
    caption = "Fuente: GIRAI 2024"
  ) +
  theme_minimal()

####################
# Mapas con leaflet #
####################

# leaflet crea mapas dinámicos a través de objetos html

# leaflet() %>%
#  addTiles() # Así abrimos un mapa base

# Configuramos una paleta de colores para el mapa
# pal <- colorNumeric("viridis", # Llamamos a una paleta ya existente
#                    domain = mapa_datos$index, # Le asignamos valores
#                   na.color = "transparent") # ¿Qué hacer para valores NA?

# Mapa interactivo
# leaflet(mapa_datos) %>% 
#  addTiles() %>% # Mapa base
  
#  addPolygons( # Agregamos los polígonos de nuestros países
#    fillColor = ~pal(index), # Coloreados según index
#    weight = 1,
#    color = "white",
#    fillOpacity = 0.7,
#    label = ~ paste(name, ": ", index) # Agregamos una etiqueta on hover
#  ) %>%
#  addLegend( # Configuramos la leyenda
#    pal = pal, # con la misma paleta de colores que usamos en el mapa
#    values = ~ index, # y los mismos valores
#    title = "Índice"
#  )


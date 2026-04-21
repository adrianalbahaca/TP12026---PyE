# Instalar paquetes de ser necesario
# install.packages("tidyselect")
# install.packages("janitor")

library(tidyselect)
library(janitor)

# Estructura del conjunto de datos
str(datos_limpios)

# Algunas medidas resumen 
summary(datos_limpios)

# Puedo fijar los datos por comodidad
attach(datos_limpios)

# Primero, obtengo un top 10 de los países con mayor puntaje en el GIRAI
top_10 <- datos_limpios %>%
  arrange(desc(GIRAI)) %>%
  select(Pais, GIRAI_region, GIRAI) %>%
  head(10)

# Luego, selecciono a los países con los 10 peores puntajes en el GIRAI
bottom_10 <- datos_limpios %>%
  arrange(GIRAI) %>%
  select(Pais, GIRAI_region, GIRAI) %>%
  head(10)

resumen_p70 <- datos_limpios %>%
  select(starts_with("p70_")) %>%
  colSums() %>%
  as.data.frame() %>%
  rownames_to_column("area") %>%
  rename(frecuencia=".") %>%
  mutate(
    porcentaje = round(frecuencia/nrow(datos_limpios) * 100, 1),
    area = str_remove(area, "p70_")
  ) %>%
  arrange(desc(frecuencia))

# -------------------------------------
# Análisis del GIRAI
girai_valores <- datos_limpios %>%
  dplyr::summarise(
    n = n(),
    Media = mean(datos_limpios$GIRAI),
    Mediana = median(datos_limpios$GIRAI),
    Desvio = sd(datos_limpios$GIRAI),
    Minimo = min(datos_limpios$GIRAI),
    Maximo = max(datos_limpios$GIRAI),
    Q1 = quantile(datos_limpios$GIRAI, 0.25),
    Q3 = quantile(datos_limpios$GIRAI, 0.75),
    dif_relativa = round(mean(GIRAI)-median(GIRAI)/mean(GIRAI)*100, 1)
  )

# Cant. de países por debajo de la media
paises_debajo <- datos_limpios %>%
  summarise(
    bajo_media = sum(GIRAI < mean(GIRAI)),
    porcentaje = round(sum(GIRAI < mean(GIRAI)) / n() * 100, 1)
  )

# --------------------
# Análisis de corte

# Se empieza por el Q3
girai_valores$Q3

datos_limpios %>%
  filter(GIRAI >= 32 & GIRAI <= 40) %>%
  select(Pais, GIRAI_region, GIRAI) %>%
  arrange(desc(GIRAI))

# No hay un quiebre natural, se desarrollan de una forma muy heterogénea
# Además, los puntajes siguen siendo bajos
# Por lo que, se elige 40 como el corte

# -------------------------------------------------
# Análisis del grupo
lideres_valores_grupo <- datos_limpios %>%
  group_by(lider) %>%
  summarise(
    Media_GOB = mean(gob),
    Media_DDHH = mean(ddhh),
    Media_CAP = mean(cap),
    Media_AG = mean(ag)
  )

# -------------------------------------------------
# Análisis p70
resumen_p70_grupo <- datos_limpios %>%
  select(lider, starts_with("p70_")) %>%
  pivot_longer(
    cols      = starts_with("p70_"),
    names_to  = "area",
    values_to = "supera70"
  ) %>%
  mutate(area = str_remove(area, "p70_")) %>%
  group_by(lider, area) %>%
  summarise(
    frecuencia = sum(supera70),
    porcentaje = round(mean(supera70) * 100, 1),
    .groups    = "drop"
  ) %>%
  arrange(lider, desc(porcentaje))

resumen_p70_grupo

# --------------------------------------------------------------------

# Otras funciones para obtener medidas

# Posición: tendencia central
# mean(datos_limpios$altura) # Media aritmética
# median(datos_limpios$altura) # Mediana

# Posición: otras
# min(altura) 
# max(altura)
# quantile(altura) # 5 medidas resumen
# quantile(altura, 0.9) # Otros percentiles
# sort(table(especie), decreasing = TRUE)[1] # Moda

# Dispersión
# range(altura) # Valores mín y max
# max(altura) - min(altura) # Rango
# sd(altura) # Desvío estándar
# var(altura) # Variancia
# IQR(altura) # Rango intercuartílico
# round(sd(altura)/mean(altura)*100,1) # Coeficiente de variación

# Otras medidas
# var(altura,diametro) # Covariancia
# cor(altura,diametro) # Correlación lineal

# Medidas por grupos
# datos_limpios %>% group_by(especie) %>%
#  summarise(Promedio = median(altura),
#            Desv.Est. = IQR(altura),
#            Mínimo = min(altura),
#           Máximo = max(altura))

# Distribuciones condicionales
# tabyl(datos_limpios, tiempo, follaje) %>%
#  adorn_totals(where = c("row", "col")) %>%
#  adorn_percentages(denominator = "row") %>%
#  adorn_pct_formatting(digits = 1) %>%
#  adorn_title(placement = "top", "Origen", "Tipo de follaje")

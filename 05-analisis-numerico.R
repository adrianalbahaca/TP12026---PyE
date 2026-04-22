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
  dplyr::arrange(desc(GIRAI)) %>%
  dplyr::select(Pais, GIRAI_region, GIRAI) %>%
  head(10)

top_10

# Luego, selecciono a los países con los 10 peores puntajes en el GIRAI
bottom_10 <- datos_limpios %>%
  dplyr::arrange(GIRAI) %>%
  dplyr::select(Pais, GIRAI_region, GIRAI) %>%
  head(10)

bottom_10

resumen_p70 <- datos_limpios %>%
  dplyr::select(starts_with("p70_")) %>%
  colSums() %>%
  as.data.frame() %>%
  tibble::rownames_to_column("area") %>%
  dplyr::rename(frecuencia=".") %>%
  dplyr::mutate(
    porcentaje = round(frecuencia/nrow(datos_limpios) * 100, 1),
    area = stringr::str_remove(area, "p70_")
  ) %>%
  dplyr::arrange(desc(frecuencia))

# -------------------------------------
# Análisis del GIRAI
girai_valores <- datos_limpios %>%
  dplyr::summarise(
    n = dplyr::n(),
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
  dplyr::summarise(
    bajo_media = sum(GIRAI < mean(GIRAI)),
    porcentaje = round(sum(GIRAI < mean(GIRAI)) / dplyr::n() * 100, 1)
  )

# --------------------
# Análisis de corte

# Se empieza por el Q3
girai_valores$Q3

datos_limpios %>%
  dplyr::filter(GIRAI >= 32 & GIRAI <= 40) %>%
  dplyr::select(Pais, GIRAI_region, GIRAI) %>%
  dplyr::arrange(desc(GIRAI))

# No hay un quiebre natural, se desarrollan de una forma muy heterogénea
# Además, los puntajes siguen siendo bajos
# Por lo que, se elige 40 como el corte

# -------------------------------------------------
# Análisis del grupo

lideres_valores_grupo <- datos_limpios %>%
  dplyr::group_by(lider) %>%
  dplyr::summarise(
    Mediana_GOB = median(gob),
    Mediana_DDHH = median(ddhh),
    Mediana_CAP = median(cap),
    Mediana_AG = median(ag),
    Mediana_ANE = median(ane),
    Desvio_GOB = sd(gob),
    Desvio_DDHH = sd(ddhh),
    Desvio_CAP = sd(cap),
    Desvio_AG = sd(ag),
    Desvio_ANE = sd(ane),
    Min_GIRAI = min(GIRAI),
    Max_GIRAI = max(GIRAI)
  )

lideres_valores_grupo

var_girai_lideres <- datos_limpios %>%
  dplyr::filter(lider == "Líder") %>%
  dplyr::summarize(
    Media = mean(GIRAI),
    Mediana = median(GIRAI),
    Desvio = sd(GIRAI),
    Q1 = quantile(GIRAI, 0.25),
    Q3 = quantile(GIRAI, 0.75),
    Rango = round(max(GIRAI) - min(GIRAI), 2),
    Coef_Var = round(sd(GIRAI) / mean(GIRAI)*100, 2)
  )

var_girai_lideres

var_girai <- datos_limpios %>%
  dplyr::group_by(lider) %>%
  dplyr::summarize(
    Media = round(mean(GIRAI), 2),
    Mediana = round(median(GIRAI), 2),
    Desvío = round(sd(GIRAI), 2),
    CV = round(sd(GIRAI)/mean(GIRAI)*100, 2)
  )

var_girai

# -------------------------------------------------
# Análisis p70
resumen_p70_grupo <- datos_limpios %>%
  dplyr::select(lider, starts_with("p70_")) %>%
  tidyr::pivot_longer(
    cols      = starts_with("p70_"),
    names_to  = "area",
    values_to = "supera70"
  ) %>%
  dplyr::mutate(area = stringr::str_remove(area, "p70_")) %>%
  dplyr::group_by(lider, area) %>%
  dplyr::summarise(
    frecuencia = sum(supera70),
    porcentaje = round(mean(supera70) * 100, 1),
    .groups    = "drop"
  ) %>%
  dplyr::arrange(lider, desc(porcentaje))

resumen_p70_grupo

# ------------------------------------------------------------------
# Brechas entre lider y resto

valores_grupo <- datos_limpios %>%
  dplyr::group_by(lider) %>%
  dplyr::summarise(
    Mediana_GOB = median(gob),
    Mediana_DDHH = median(ddhh),
    Mediana_CAP = median(cap),
    Mediana_AG = median(ag),
    Mediana_ANE = median(ane)
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("Mediana_"),
    names_to = "Pilar",
    values_to = "Mediana"
  ) %>%
  tidyr::pivot_wider(
    names_from = lider,
    values_from = Mediana
  ) %>%
  dplyr::mutate(
    brecha = round(Líder - Resto, 2),
    pilar = stringr::str_remove(Pilar, "Mediana_")
  )

valores_grupo

comp_gob_ane <- datos_limpios %>%
  dplyr::group_by(lider) %>%
  dplyr::summarize(
    Mediana_GOB = round(median(gob), 2),
    Mediana_ANE = round(median(ane), 2),
    Ratio = round(median(ane) / median(gob), 2)
  )

comp_gob_ane

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

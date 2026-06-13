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
# Análisis del GIRAI (Cuantitativa continua)
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
    rang_intercuantil = IQR(datos_limpios$GIRAI),
    dif_relativa = round(((mean(GIRAI)-median(GIRAI))/mean(GIRAI))*100, 1)
  )

girai_valores

# Nótese que el valor del desvío estándar es del 20.4, un valor demasiado alto
# Note que el rango intercualítico es del 30.2, nos indica valores dispersos
# en el medio
# Esta base de datos tiene puntajes en el GIRAI bastante dispersos
# --------------------
# Análisis de corte

# Se empieza por la mediana, por un desvío estándar alto
girai_valores$Mediana
# Se nota un puntaje demasiado bajo para usar como corte de 'desarrollado'
# Además, note la diferencia relativa
girai_valores$dif_relativa
# La media supera a la mediana en un valor relativamente alto

girai_valores$rang_intercuantil
# El 50% de los puntajes están en un rango de 30 puntos, mostrando datos
# centrales dispersos aún

# Conclusión: Usar la mediana no es una buena referencia. Para hacer la def.
q3_girai <- quantile(datos_limpios$GIRAI, 0.75)

rm(q3_girai)
q3_girai <- quantile(datos_limpios$GIRAI, 0.75)

class(datos_limpios)

paises_desarrollados <- datos_limpios %>%
  dplyr::filter(GIRAI > q3_girai)

paises_subdesarrollados <- datos_limpios %>%
  dplyr::filter(GIRAI <= q3_girai)

paises_desarrollados
paises_subdesarrollados

# --------------------------------------------------------------------
# Análisis de gob vs cap en países desarrollados
pd_gob_cap <- paises_desarrollados %>%
  dplyr::summarise(
    Media_gob = mean(gob),
    Mediana_gob = median(gob),
    IQR_gob = IQR(gob),
    Sd_gob = sd(gob),
    Media_cap = mean(cap),
    Mediana_cap = median(cap),
    IQR_cap = IQR(cap),
    Sd_cap = sd(cap)
  )

psd_gob_cap <- paises_subdesarrollados %>%
  dplyr::summarise(
    Media_gob = mean(gob),
    Mediana_gob = median(gob),
    IQR_gob = IQR(gob),
    Sd_gob = sd(gob),
    Media_cap = mean(cap),
    Mediana_cap = median(cap),
    IQR_cap = IQR(cap),
    Sd_cap = sd(cap)
  )

pd_gob_cap
psd_gob_cap

# ----------------------------------------------------------------
niveles <- c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto")

moda_sec_ag <- names(sort(table(datos_limpios$sec_ag), decreasing=TRUE))[1]
moda_sec_ane <- names(sort(table(datos_limpios$sec_ane), decreasing=TRUE))[1]

mediana_sec_ag <- median(as.numeric(factor(datos_limpios$sec_ag, levels=niveles)), na.rm=TRUE)
mediana_sec_ane <- median(as.numeric(factor(datos_limpios$sec_ane, levels=niveles)), na.rm=TRUE)

mediana_sec_ag
mediana_sec_ane
moda_sec_ag
moda_sec_ane

# ----------------------------------------------------------------------------
# Comparativa entre países desarrollados y países subdesarrollados
# Desarrollados
paises_desarrollados <- datos_limpios %>%
  dplyr::filter(GIRAI > q3_girai) %>%
  dplyr::summarise(
    mediana_gob = median(gob, na.rm=TRUE),
    mediana_cap = median(cap, na.rm=TRUE),
    mediana_areas_ag = median(areas_ag, na.rm=TRUE),
    mediana_areas_ane = median(areas_ane, na.rm=TRUE),
    prop_p70_laboral = mean(p70_laboral, na.rm=TRUE),
    prop_p70_datpers = mean(p70_datpers, na.rm=TRUE),
    prop_p70_segu = mean(p70_segu, na.rm=TRUE)
  )

# Subdesarrollados
paises_subdesarrollados <- datos_limpios %>%
  dplyr::filter(GIRAI <= q3_girai) %>%
  dplyr::summarise(
    mediana_gob = median(gob, na.rm=TRUE),
    mediana_cap = median(cap, na.rm=TRUE),
    mediana_areas_ag = median(areas_ag, na.rm=TRUE),
    mediana_areas_ane = median(areas_ane, na.rm=TRUE),
    prop_p70_laboral = mean(p70_laboral, na.rm=TRUE),
    prop_p70_datpers = mean(p70_datpers, na.rm=TRUE),
    prop_p70_segu = mean(p70_segu, na.rm=TRUE)
  )

paises_desarrollados
paises_subdesarrollados
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

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
  dplyr::select(Pais, GIRAI_region, GIRAI, cap, gob, ddhh) %>%
  head(10)

# Luego, selecciono a los países con los 10 peores puntajes en el GIRAI
bottom_10 <- datos_limpios %>%
  dplyr::arrange(GIRAI) %>%
  dplyr::select(Pais, GIRAI_region, GIRAI, cap, gob, ddhh) %>%
  head(10)

# Medición del p70 en los países

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

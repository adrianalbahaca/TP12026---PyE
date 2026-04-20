# Instalo los paquetes necesarios (si aún no los tengo instalados)
install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# Fijo el dataset
attach(datos)

# ---------------------------
# Modificar datos
# ---------------------------
datos_limpios <- datos %>%
  select(
    # Identificación
    Pais, ISO3, GIRAI_region,
    
    # Cuantitativas continuas
    GIRAI,
    
    # Cuantitativa discreta
    areas_concient,
    
    # Categórica escala nominal
    tipo_privado_es,
    
    # Categórica escala ordinal
    sec_ag,
    
    # Categórica de respuesta múltiple
    p70_sesgo, p70_infancia, p70_divers, p70_datpers,
    p70_genero, p70_suphum, p70_transp, p70_laboral, p70_segu, p70_transp
  )
  mutate (
    # Darle orden a variables cat. escala ordinal
    sec_ag = factor(sec_ag, 
                    levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                    ordered=TRUE)
    
  )

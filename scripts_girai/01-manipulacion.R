# Instalo los paquetes necesarios (si aún no los tengo instalados)
install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# ---------------------------
# Modificar datos
# ---------------------------
datos_limpios <- datos %>%
  select(
    # Identificación
    Pais, ISO3, GIRAI_region, GIRAI,
    sec_ag, sec_ane, gob, cap, `Dimensión mejor puntuada`,
    p70_transp, p70_laboral, p70_segu, p70_datpers,
    areas_ag, areas_ane
    # Sección
    
  ) %>%
  mutate (
    # Darle orden a variables cat. escala ordinal
    sec_ag = factor(sec_ag,
                    levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                    ordered=TRUE),
    sec_ane = factor(sec_ane,
                     levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                     ordered=TRUE)
  )


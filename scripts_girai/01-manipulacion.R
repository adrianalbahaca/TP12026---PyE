# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")

# Cargo los paquetes que voy a usar
library(tidyverse)

# ---------------------------
# Modificar datos
# ---------------------------
datos_limpios <- datos %>%
  select(
    # Identificación
    Pais, ISO3, GIRAI_region,

    # Cuantitativas continuas
    GIRAI, ddhh, gob, cap, ag, ane, mng,

    # Cuantitativa discreta
    areas_concient, areas_mng, areas_ane,

    # Categórica escala nominal
    tipo_privado_es, tipo_academia_es,

    # Categórica escala ordinal
    sec_ag, sec_ane, sec_mng,

    # Categórica de respuesta múltiple
    p70_sesgo, p70_infancia, p70_divers, p70_datpers,
    p70_genero, p70_suphum, p70_transp, p70_laboral, p70_segu, p70_transp
    
  ) %>%
  mutate (
    # Darle orden a variables cat. escala ordinal
    sec_ag = factor(sec_ag,
                    levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                    ordered=TRUE),
    sec_ane = factor(sec_ane,
                     levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                     ordered=TRUE),
    lider = factor(ifelse(GIRAI > 40, "Líder", "Resto")),
    tipo_privado_es = replace_na(tipo_privado_es, "Sin iniciativas")
    # Uso de 40 por Q3, medida conservadora
  )


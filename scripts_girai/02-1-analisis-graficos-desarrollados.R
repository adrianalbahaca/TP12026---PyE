# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

theme_set(theme_minimal(base_size=25))

# Bivariado - cuantitativa y cuantitativa
plot_bivariado_gob_cap <- paises_desarrollados %>%
  filter(!is.na(gob), !is.na(cap)) %>%
  ggplot(aes(x=cap, y=gob)) +
  geom_point(color="#69b3a2", size=2.5, alpha=0.7) +
  geom_abline(slope=1, intercept=0, color="#ef476f", linetype="dashed", linewidth=0.8) +
  labs(
    title="Relación entre gobernanza y capacidades en IA",
    x="Capacidades",
    y="Gobernanza"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_bivariado_gob_cap

# Bivariado - cualitativa y cualitativa
plot_sectores <- paises_desarrollados %>%
  select(sec_ag, sec_ane) %>%
  filter(!is.na(sec_ag), !is.na(sec_ane)) %>%
  pivot_longer(
    cols = everything(),
    names_to="Sector",
    values_to="Nivel"
  ) %>%
  mutate(
    Sector = case_when(
      Sector == "sec_ag" ~ "Acciones gubernamentales",
      Sector == "sec_ane" ~ "Actores no estatales"
    )
  ) %>%
  
  ggplot(aes(x=Nivel, fill=Sector)) +
  geom_bar(position="dodge", alpha=0.9, color="#e9ecef", linewidth=0.2) +
  scale_fill_manual(values=c("Acciones gubernamentales" = "#69b3a2",
                             "Actores no estatales" = "#4682b4")) +
  labs(
    title="Importancia brindada al uso responsable de la IA según cada sector",
    x="Nivel de desarrollo",
    y="Cantidad de países",
    fill="Sector analizado"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_sectores

# Bivariado - cualitativa y cuantitativa
plot_boxplot_mng <- paises_desarrollados %>%
  filter(!is.na(sec_mng), !is.na(gob)) %>% 
  mutate(sec_mng = factor(sec_mng, levels = c("Bajo", "Medio", "Alto", "Muy alto"))) %>%
  ggplot(aes(x = sec_mng, y = gob, fill = sec_mng)) +
  
  geom_boxplot(alpha = 0.8, color = "#555555", outlier.colour = "#ef476f", show.legend = FALSE) +
  scale_fill_manual(values = rep("#69b3a2", 5)) +
  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#ef476f", show.legend = FALSE) +
  
  stat_summary(fun = median, geom = "text", 
               aes(label = round(after_stat(y), 1)), # Redondea a 1 decimal
               vjust = -0.6,                         # Lo sube un poquito para que no pise la línea
               color = "#2b2b2b", 
               fontface = "bold", 
               size = 3.8) +
  
  labs(
    title = "Distribución del índice de gobernanza según el nivel de marcos gubernamentales",
    subtitle = "La línea central indica la mediana (valor en texto) y el rombo rosa representa el promedio",
    x = "Nivel de desarrollo en marcos gubernamentales",
    y = "Índice de gobernanza en la IA"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold"),
  )

plot_boxplot_mng

# Univariado - categórica nominal
tabla_frecuencias_dimension <- table(paises_desarrollados$`Dimensión mejor puntuada`)
moda_dimension <- names(tabla_frecuencias_dimension)[which.max(tabla_frecuencias_dimension)]
plot_dimension_mejor_puntuada <- paises_desarrollados %>%
  ggplot(aes(x=`Dimensión mejor puntuada`)) +
  geom_bar(aes(fill=`Dimensión mejor puntuada` == moda_dimension),
           color="#e9ecef", alpha=0.9, width=0.2, show.legend=FALSE) +
  scale_x_discrete(labels=c("cap" = "Capacidades", "ddhh" = "Derechos humanos",
                            "gob" = "Gobernanza")) +
  scale_fill_manual(values=c("TRUE"="#69b3a2", "FALSE"="#b0bec5")) +
  annotate("text", 
           x = moda_dimension, 
           y = max(tabla_frecuencias_dimension) + (max(tabla_frecuencias_dimension) * 0.05),
           label = "Moda", 
           color = "#69b3a2", 
           fontface = "bold", 
           size = 4.5, 
           hjust = 0.5) +
  labs(
    title="Prioridades del sector público en IA responsable en países desarrollados",
    x="Dimensión evaluada",
    y="Cantidad de países"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_dimension_mejor_puntuada

# Univariado - cuantitativa discreta
frecuencia_ag <- paises_desarrollados %>%
  filter(!is.na(areas_ag)) %>%
  count(areas_ag)

plot_areas_ag <- frecuencia_ag %>%
  ggplot(aes(x=areas_ag, y=n)) + 
  geom_segment(aes(xend=areas_ag, yend=0), color="#69b3a2", linewidth=1.2) +
  scale_x_continuous(breaks=0:19, limits=c(0,19)) +
  labs(
    title="Cantidad de áreas con acción gubernamental en IA en países desarrollados",
    subtitle="Distribución de frecuencias para el sector público",
    x="Cantidad de áreas con intervención",
    y="Cantidad de países"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_areas_ag

# Univariado - cuantitativa discreta
frecuencia_ane <- paises_desarrollados %>%
  filter(!is.na(areas_ane)) %>%
  count(areas_ane)

plot_areas_ane <- frecuencia_ane %>%
  ggplot(aes(x=areas_ane, y=n)) + 
  geom_segment(aes(xend=areas_ane, yend=0), color="#69b3a2", linewidth=1.2) +
  scale_x_continuous(breaks=0:19, limits=c(0,19)) +
  labs(
    title="Cantidad de áreas con acción no estatal en IA en países desarrollados",
    subtitle="Distribución de frecuencias para actores no estatales",
    x="Cantidad de áreas con intervención",
    y="Cantidad de países"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_areas_ane

# Univariado - respuesta múltiple
plot_indices <- paises_desarrollados %>%
  select(p70_laboral, p70_datpers, p70_segu) %>%
  filter(!is.na(p70_laboral), !is.na(p70_datpers), !is.na(p70_segu)) %>%
  pivot_longer(
    cols = everything(),
    names_to="Indice",
    values_to="Resultado"
  ) %>%
  mutate(
    Indice = case_when(
      Indice == "p70_laboral" ~ "Protección Laboral",
      Indice == "p70_datpers" ~ "Datos Personales y Privacidad",
      Indice == "p70_segu" ~ "Seguridad, Precisión y Fiabilidad",
    )
  ) %>%
  
  ggplot(aes(x=Indice, fill=as.factor(Resultado))) +
  geom_bar(position="dodge", alpha=0.9, color="#e9ecef", linewidth=0.2) +
  scale_fill_manual(values=c("1" = "#69b3a2", "0" = "#b0bec5"),
                    labels=c("1" = "Supera el umbral", "0" = "No supera el umbral")) +
  labs(
    title="Preocupaciones regulatorias para la IA en países desarrollados",
    x="Indicador analizado",
    y="Cantidad de países",
    fill="Condición del índice"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_indices

# Bivariado - cualitativa y cuantitativa
plot_boxplot_gob <- paises_desarrollados %>%
  filter(!is.na(sec_ag), !is.na(gob)) %>% 
  ggplot(aes(x = sec_ag, y = gob, fill = sec_ag)) +
  
  geom_boxplot(alpha = 0.8, color = "#555555", outlier.colour = "#ef476f", show.legend = FALSE) +
  scale_fill_manual(values = rep("#69b3a2", 5)) +
  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#ef476f", show.legend = FALSE) +
  
  stat_summary(fun = median, geom = "text", 
               aes(label = round(after_stat(y), 1)), # Redondea a 1 decimal
               vjust = -0.6,                         # Lo sube un poquito para que no pise la línea
               color = "#2b2b2b", 
               fontface = "bold", 
               size = 3.8) +
  
  labs(
    title = "Distribución del índice de gobernanza según el nivel de acciones públicas",
    subtitle = "La línea central indica la mediana (valor en texto) y el rombo rosa representa el promedio",
    x = "Nivel de desarrollo en acciones gubernamentales",
    y = "Índice de gobernanza en la IA"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, face = "italic"),
    axis.text.x = element_text(size = 10, face = "bold"),
  )

plot_boxplot_gob

# Univariado - categorica ordinal
plot_sec_ag <- paises_desarrollados %>%
  ggplot(aes(x=sec_ag)) +
  geom_bar(fill="#69b3a2", alpha=0.9, width=0.2, show.legend=FALSE) +
  labs(
    title="Nivel de desarrollo de acciones gubernamentales respecto al uso responsable de IA",
    x="Nivel de desarrollo",
    y="Cantidad de países"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size=15, face="bold")
  )

plot_sec_ag

plots <- list(
  "pd_gob_cap" = plot_bivariado_gob_cap,
  "pd_boxplot_gob" = plot_boxplot_gob,
  "pd_sec_ag_sec_ane" = plot_sectores,
  "pd_boxplot_mng" = plot_boxplot_mng,
  "plot_areas_ag_pd" = plot_areas_ag,
  "plot_sec_ag_pd" = plot_sec_ag,
  "pd_indices" = plot_indices,
  "pd_sectores" = plot_sectores
)

ruta <- "/home/aathinkpad/Documentos/Code/Universidad/PyE/TP12026---PyE/scripts_girai"

for (nombre in names(plots)) {
  ggsave(paste0(ruta, nombre, ".png"), plots[[nombre]], width=10, height=6, dpi=150)
}

plot_girai <- ggplot(datos_limpios, aes(x = GIRAI)) +
  geom_histogram(fill = "#69b3a2", color = "white", bins = 20) +
  geom_vline(xintercept = q3_girai, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = q3_girai + 3, y = 15, label = "Umbral: 32.9", color = "red", size = 5) +
  labs(title = "Distribución del índice GIRAI 2024",
       x = "Puntaje GIRAI", y = "Cantidad de países") +
  theme_minimal(base_size = 18)

plot_girai
ggsave(paste0(ruta, "plot_girai.png"), plot_girai, width=10, height=6, dpi=150)

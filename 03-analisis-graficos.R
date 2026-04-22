# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("tidyverse")
# install.packages("ggplot2")

# Cargo los paquetes que voy a usar
library(tidyverse)
library(ggplot2)

# Variable categórica medida en escala nominal
grafico_tipo_privado <- datos_limpios %>%
  count(tipo_privado_es) %>%
  mutate(tipo_privado_es = reorder(tipo_privado_es, n)) %>%
  ggplot(aes(x=n, y=tipo_privado_es)) +
  labs(title="Distribución de iniciativas por país",
       x="Cantidad de países",
       y="Tipo de iniciativas de parte del sector privado") +
  geom_bar(stat="identity", fill="#2a9df0", width=0.5) +
  geom_text(aes(label=n), hjust=-0.3, size=4, color="black")

# Variables categóricas medidas en escala ordinal
grafico_sec_ag <- datos_limpios %>%
  count(sec_ag) %>%
  ggplot(aes(x=n, y=sec_ag)) +
  labs(title="Nivel de desarrollo de acciones gubernamentales para el uso responsable de IA",
       x="Cantidad de países con acciones gubernamentales",
       y="Nivel de desarrollo") +
  geom_bar(stat="identity", fill="#ceac48", width=0.4) +
  geom_text(aes(label=n), hjust=-0.3, size=4, color="black")

grafico_sec_ane <- datos_limpios %>%
  count(sec_ane) %>%
  ggplot(aes(x=n, y=sec_ane)) +
  labs(title="Nivel de desarrollo de actores no estatales respecto al uso responsable de la IA",
       x="Cantidad de países  con acciones de actores no estatales",
       y="Nivel de desarrollo") +
  geom_bar(stat="identity", fill="#ceac48", width=0.4) +
  geom_text(aes(label=n), hjust=-0.3, size=4, color="black")

# Variable categórica de respuesta múltiple
grafico_p70 <- datos_limpios %>%
  pivot_longer(
    cols=starts_with("p70_"),
    names_to="area_tematica",
    values_to="puntaje",
  ) %>%
  filter(puntaje==1) %>%
  count(area_tematica) %>%
  mutate(area_tematica = case_when( # Queremos un texto más lindo para el gráfico
           area_tematica == "p70_sesgo" ~ "Sesgo y Discriminación Injusta",
           area_tematica == "p70_infancia" ~ "Derechos de la Infancia",
           area_tematica == "p70_divers" ~ "Diversidad Cultural y Lingüistica",
           area_tematica == "p70_datpers" ~ "Protección de Datos y Privacidad",
           area_tematica == "p70_genero" ~ "Igualdad de Género",
           area_tematica == "p70_suphum" ~ "Supervisión Humana",
           area_tematica == "p70_transp" ~ "Transparencia y Explicabilidad",
           area_tematica == "p70_laboral" ~ "Protección Laboral y Derecho al Trabajo",
           area_tematica == "p70_segu" ~ "Seguridad, Precisión y Fiabilidad"
          ), area_tematica = reorder(area_tematica, n)) %>%
  ggplot(aes(x=n, y=area_tematica)) +
  labs(title="Distribución de países con puntajes mayores a 70",
       x="Cantidad de países",
       y="Tipos de índices") +
  geom_bar(stat="identity", fill="#ffd248", width=0.5) +
  geom_text(aes(label=n), hjust=-0.3, size=4, color="black")

# Variable cuantitativa discreta
grafico_areas_concient <- datos_limpios %>%
  count(areas_concient) %>%
  ggplot(aes(x=areas_concient, y=n)) +
  labs(title="Cantidad de áreas sobre las que se hicieron eventos y concienciación pública",
       x="Cantidad de áreas",
       y="Cántidad de países") +
  geom_bar(stat="identity", fill="#2a9df0", width=0.06) +
  geom_text(aes(label=n), hjust=-0.3, size=4, color="black")

# Variable cuantitativa continua
grafico_indices <- datos_limpios %>%
  pivot_longer(
    cols=c(GIRAI, ddhh, gob, cap, ag),
    names_to="indice",
    values_to="puntaje",
  ) %>%
  mutate(indice=case_when(
    indice == "ag" ~ "Acciones gubernamentales",
    indice == "cap" ~ "Capacidades en IA",
    indice == "ddhh" ~ "Derechos Humanos",
    indice == "GIRAI" ~ "Uso responsable de IA",
    indice == "gob" ~ "Gobernanza"
  )) %>%
  ggplot(aes(x=indice, y=puntaje, fill=indice)) +
  geom_boxplot() +
  labs(title="Comparación de Índices", x="Tipo de Índice", y="Puntaje obtenido")

# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googlesheets4")

library(googlesheets4)

# Link al archivo
url="https://docs.google.com/spreadsheets/d/1UwMDUPScSdUJ2KoyNNZJyE22d7l1wbY40v_TdJtJ0AM/edit?gid=1466735819#gid=1466735819"

# Evito loggeo
gs4_deauth()

# Leo el archivo y almaceno los datos en un data frame
datos <- read_sheet(url, skip = 2)

# Veo la estructura del dataset
str(datos)

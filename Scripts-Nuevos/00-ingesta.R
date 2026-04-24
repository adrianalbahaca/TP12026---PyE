# install.packages("googlesheets4")
library(googlesheets4)

gs4_deauth()
url="https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=1481484702#gid=1481484702"

datos <- read_sheet(url, skip=1, sheet=2)

#Cargamos los datos de excel

library(readxl)
library(tidyverse)
library(tidyr)
library(maps)
library(pacman)
pacman::p_load(here, easyclimate, patchwork, viridis, ggmisc, fs, future, furrr, rmarkdown) 
library(remotes)
library(easyclimate)

polif <- read_csv2(file = here("DATOS", "polifenoles.csv"))
produ<- read_csv2(file = here("DATOS", "produccion.csv"))

names(polif)

library(ggplot2)
names(polif)

c("ggplot2", "readr", "dplyr", "ggThemeAssist", "patchwork", "titanic", "easyclimate", "terra", "sf", "ggmap", "plotly") %in% rownames(installed.packages())
c("promises","mime", "cachem") %in% rownames(installed.packages())


ggplot() + 
  geom_point(data = polif, aes(x =UdExp, y = Polifenoles)) 

ggplot() + 
  geom_point(data = polif, aes(x = as.factor(Riego), y = Polifenoles))

ggplot() + 
  geom_jitter(data = polif, aes(x = as.factor(Riego), y = Polifenoles, color = Cubierta))
              

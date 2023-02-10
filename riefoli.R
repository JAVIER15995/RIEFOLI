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
  geom_point(data = polif, aes(x = as.factor(Riego), y = Polifenoles, color = Cubierta))

ggplot() + 
  geom_point(data = produ, aes(x = as.factor(Riego), y = Produccion, color = Cubierta))          
names(produ)

ggplot() + 
  geom_point(data = polif, aes(x = as.factor(Cubierta), y = Polifenoles, color = Riego))
ggplot() + 
  geom_point(data = produ, aes(x = as.factor(Cubierta), y = Produccion, color =Riego))      
names(produ)


##tabla con las medias de las repeticiones

mean_polif <- polif |>
  group_by(UdExp) |>
  summarise(mean_polif= mean(Polifenoles), sd_polif = sd(Polifenoles))


names(polif)

mean_produ <- produ |>
  group_by(Bloque, UdExp, Riego, Cubierta) |>
  summarise(mean_produc= mean(Produccion), sd_produ = sd(Produccion))

# con group by añado las columnas Bloque, Cubierta, Riego  

#Ahora fusiono ambas tablas

riefoli <-full_join(mean_produ, mean_polif, by =c("UdExp"))

Riefoli_trat <- 


  

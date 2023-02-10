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


## para saber el total d media minimo y max de kg de aceitunas que cogimos
Kg <- glimpse(produ$Produccion)

mean(produ$Produccion)
sum(produ$Produccion)
sd(produ$Produccion)

# con group by añado las columnas Bloque, Cubierta, Riego  

#Ahora fusiono ambas tablas

riefoli <-full_join(mean_produ, mean_polif, by =c("UdExp"))

Riefoli_trat <- riefoli |> 
  unite(trat, c(Riego, Cubierta), remove = FALSE, sep = "_")

# Creo una nueva columna de tratamiento uniendo riego y cubierta

names (Riefoli_trat)

ggplot() + 
  geom_point(data =Riefoli_trat , aes(x = as.factor(Cubierta), y = mean_polif, color = Riego))
ggplot() + 
  geom_point(data = Riefoli_trat, aes(x = as.factor(Cubierta), y = mean_produc, color =Riego))      
names(produ)
ggplot() + 
  geom_point(data = Riefoli_trat, aes(x = as.factor(trat), y = mean_produc, color =Riego))      

names(Riefoli_trat)

#grafico de barras


summary(Riefoli_trat)
str (Riefoli_trat)



ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_produc, fill = Riego)) +
  geom_boxplot()
ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_produc, fill = Cubierta)) +
  geom_boxplot()

ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_polif, fill = Riego)) +
  geom_boxplot()

ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_polif, color = Riego)) +
  geom_point( size= 4 )+
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)


ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, color = Riego)) +
  geom_boxplot()
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
  
  ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles)) +
    geom_boxplot()
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
  
  ggplot(data = polif , aes(x =as.factor(Riego), y = Polifenoles)) +
    geom_boxplot()
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)


geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc), width = 0.25)

ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_polif)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif), width = 0.25)

##Hacer esto mismo con producción

ggplot(data = Riefoli_trat) + 
  geom_violin(aes(x = as.factor(trat), y =mean_polif  , fill = Cubierta), draw_quantiles = 0.5) 



  

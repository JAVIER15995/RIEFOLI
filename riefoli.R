#Cargamos los datos de excel

library(readxl)
library(tidyverse)
library(tidyr)
library(maps)
library(pacman)
library(remotes)
library(easyclimate)
library(here)
library(ggmap)
library(dplyr)
library(plotly)
library(patchwork) 
library(easyclimate)
library(terra)

polif <- read_csv2(file = here("DATOS", "polifenoles.csv"))
produ<- read_csv2(file = here("DATOS", "produccion.csv"))

names(polif)

library(ggplot2)
names(polif)

c("ggplot2", "readr", "dplyr", "ggThemeAssist", "patchwork", "titanic", "easyclimate", "terra", "sf", "ggmap", "plotly") %in% rownames(installed.packages())
c("promises","mime", "cachem") %in% rownames(installed.packages())
 names(polif)

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

#Grafico de barras


summary(Riefoli_trat)
str (Riefoli_trat)

##Por tratamiento

ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_polif)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif), width = 0.25)
ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_produc)) +
  geom_col(fill = "black") +
  geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc), width = 0.25)


##Por factor riego

ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_produc, fill = Riego)) +
  geom_boxplot()

ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_polif, fill = Riego)) +
  geom_boxplot()

##Por factor cubierta

ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_produc, fill = Cubierta)) +
  geom_boxplot()

ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_polif, fill = Cubierta)) +
  geom_boxplot()

#Representando ambos factores

ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_polif, color = Riego)) +
  geom_point( size= 4 )+
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)

##Como solo son 16 datos, no puedo crear otro gráfico que no sea de puntos sin que me de error
#voy a los datos originales para poder hacer boxplot con las repteciones de campo o labo en lugar de con la media

ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, color = Riego)) +
  geom_boxplot()
ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, fill = Riego)) +
  geom_boxplot()
ggplot(data = produ , aes(x =as.factor(Cubierta), y = Produccion, fill = Riego)) +
  geom_boxplot()
  
##Represento las mismas tablas que al principio por factor pero con los datos de las tablas originales 
ggplot(data = polif , aes(x =as.factor(Riego), y = Polifenoles, color = Riego)) +
    geom_boxplot()
ggplot(data = polif , aes(x =as.factor(Riego), y = Polifenoles, fill = Riego)) +
  geom_boxplot()
ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, fill = Cubierta)) +
  geom_boxplot()
ggplot(data = produ, aes(x =as.factor(Riego), y = Produccion, fill = Riego)) +
  geom_boxplot()
ggplot(data = produ , aes(x =as.factor(Cubierta), y = Produccion, fill = Cubierta)) +
  geom_boxplot()

ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_polif)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif), width = 0.25)

ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_produc)) +
  geom_col(fill = "darkolivegreen4") +
  geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc), width = 0.25)

#Guardo los gráficos que me gustan

Barraspolif <- ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_polif)) +
  geom_col(fill = "cyan", color ="black") +
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
Barraspolif
Barrasprodu <- ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_produc)) +
  geom_col(fill = "darkolivegreen4") +
  geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc - sd_produ), width = 0.25)
Barrasprodu
sum_produc_riego <- ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_produc, fill = Riego)) +
  geom_boxplot()
sum_produc_riego
sum_poli_riego <- ggplot(data = Riefoli_trat, aes(x =as.factor(Riego), y = mean_polif, fill = Riego)) +
  geom_boxplot()
sum_poli_riego
sum_produc_cub <-  ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_produc, fill = Cubierta)) +
  geom_boxplot()
sum_poli_cub <-  ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_polif, fill = Cubierta)) +
  geom_boxplot()
sum_produc_cub
sum_poli_cub
Puntos_poli <-  ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_polif, color = Riego)) +
  geom_point( size= 4 )+
  geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
Puntos_poli
Puntos_Produ <-  ggplot(data = Riefoli_trat, aes(x =as.factor(Cubierta), y = mean_produc, color = Riego)) +
  geom_point( size= 4 )+
  geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc - sd_produ), width = 0.25)
Puntos_Produ
box_poli <- ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, fill = Riego)) +
  geom_boxplot()
box_poli
box_produ <- ggplot(data = produ , aes(x =as.factor(Cubierta), y = Produccion, fill = Riego)) +
   geom_boxplot()
box_produ
poli_riego <- ggplot(data = polif , aes(x =as.factor(Riego), y = Polifenoles, fill = Riego)) +
  geom_boxplot()
poli_riego
poli_cub <- ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles, fill = Cubierta)) +
  geom_boxplot()
poli_cub
produc_riego <- ggplot(data = produ, aes(x =as.factor(Riego), y = Produccion, fill = Riego)) +
  geom_boxplot()
produc_riego
produc_cub <-ggplot(data = produ , aes(x =as.factor(Cubierta), y = Produccion, fill = Cubierta)) +
  geom_boxplot()
produc_cub

#Comparo craficos hechos con el resumen y con la tabla total y guardo los que me gustan
Puntos_poli+box_poli
Puntos_poli/box_poli
comparacion_poli <- Puntos_poli+box_poli
comparacion_produ <-Puntos_Produ+box_produ
comparación_poli_riego <- sum_poli_riego+poli_riego
comparación_poli_cub <- sum_poli_cub+poli_cub
comparación_produ_riego <- sum_produc_riego+produc_riego
comparación_produ_cub <- sum_produc_cub+produc_cub

comparacion_poli 
comparacion_produ 
comparación_poli_riego 
comparación_poli_cub 
comparación_produ_riego 
comparación_produ_cub 

#Presento composiciones finales finales

Final_trat_poli <- box_poli+ Barraspolif
Final_trat_produ <- box_produ + Barrasprodu

Final_poli <- poli_riego+poli_cub
Final_produ <- produc_riego+produc_cub
Polifenoles <- Final_poli/box_poli
Produccion <- Final_produ/box_produ

#Visualización de gráficas finales

Final_trat_poli
Final_trat_produ 
Final_poli
Final_produ
Polifenoles
Produccion



##pruebas que no funcionan

# ggplot(data = Riefoli_trat) + 
#   geom_violin(aes(x = as.factor(trat), y =mean_polif  , fill = Cubierta), draw_quantiles = 0.5) 
# 
# .ggplot(data = polif , aes(x =as.factor(Cubierta), y = Polifenoles)) +
#   geom_boxplot()
# geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
# 
# geom_errorbar(aes(ymax = mean_produc + sd_produ, ymin = mean_produc), width = 0.25)
# geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
# geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif - sd_polif), width = 0.25)
# 
# ggplot(data = Riefoli_trat, aes(x =as.factor(trat), y = mean_polif)) +
#   geom_col() +
#   scale_color_manual(values = c("darkgreen", "chartreuse"))+
#   geom_errorbar(aes(ymax = mean_polif + sd_polif, ymin = mean_polif), width = 0.25)




  

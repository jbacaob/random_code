#Librerías a utilizar
library(raster)
library(sp)
library(rgeos)
library(leaflet)
library(mapview)
library(zoo)
library(broom)
library(ggplot2)
library(readxl)
library(tidyverse)
library(ggh4x)
library(maps)

#Creación de tema para gráficos
theme_jb <- function(){
  theme(
    #Titles
    plot.background = element_rect(fill = "transparent", color = NULL),
    #Panel
    panel.background = element_rect(fill = "transparent", color = NULL),
    panel.grid = element_line(color = NULL),
    #Legend
    legend.position = "bottom",
    legend.key.height= unit(0.75, 'cm'),
    legend.key.width= unit(1.25, 'cm'),
    #Axes
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    #Text
    text = element_text(size=14)
  )
  }

#Extracción de datos
nic_tot <- tidy(raster::getData("GADM", country = "Nicaragua", level = 0))

nic_dep <- raster::getData("GADM", country = "Nicaragua", level = 1)
plot(nic_dep)
nic_mun <- raster::getData("GADM", country = "Nicaragua", level = 2)
plot(nic_mun)

nic_dep@polygons[[19]] <- nic_mun@polygons[[86]] #Agregar info de Lago Managua a coordenadas departamentales

#Creación de df con departamentos e identificadores
dep_id <- unique(nic_dep@data['NAME_1'])
dep_id['id'] <- rownames(dep_id)
rownames(dep_id) <- seq(1:nrow(dep_id))

mapa_nic <- tidy(nic_dep)
mapa_nic[mapa_nic['id'] == '12924', 'id'] <- '19' #Cambiar id para Lago Managua
mapa_nic <- merge(mapa_nic, dep_id, by = 'id', all.x = TRUE)
mapa_nic[is.na(mapa_nic['NAME_1']) == TRUE, 'NAME_1'] <- 'Lago Managua'
mapa_nic$id <- factor(mapa_nic$id, levels = c("1",  "11", "12", "13", "14", "15",
                                              "16", "17", "18", "2",  "3",  "4",  "5",  "6",  "7",  "8", "9", "10", "19"))
mapa_nic <- mapa_nic %>% arrange(id, order)
mapa_nic$group <- as.character(mapa_nic$group)
mapa_nic[(mapa_nic['group'] == '12924.1') | (mapa_nic['group'] == '12924.2'), 'group'] <- '5.1' #Un solo grupo para coordenadas de Lago Managua

#Creación de nuevas coordenadas para Managua (sin lago)
mng <- mapa_nic[(mapa_nic$NAME_1 == "Managua"), ] ## Coordinates for the dep. of Managua
lake_mng <- mapa_nic[(mapa_nic$NAME_1 == "Lago Managua"), ] ##Coordinates for Lake Managua

mng1 <- mng[1:1335, ]
mng2a <- lake_mng[1:90, ]
mng2a <- mng2a[nrow(mng2a):1, ] #Revertir orden de coordenadas
mng2b <- lake_mng[191:397,] #Se excluyen coordenadas de Isla del Amor (presente en mng3)
mng2b <- mng2b[nrow(mng2b):1, ] #Revertir orden de coordenadas
mng3 <- mng[1438:2299, ]
new_mng <- rbind(mng1, mng2a, mng2b, mng3)
new_mng['order'] <- seq(1:nrow(new_mng))
new_mng[new_mng['NAME_1'] == 'Lago Managua', "NAME_1"] <- 'Managua'
new_mng[new_mng['id'] == '19', "id"] <- '5' #¡Listo!

#Adición de nueva estructura de Managua a mapa

mapa_nic <- mapa_nic %>% dplyr::filter((NAME_1 != 'Managua') & (NAME_1 != 'Lago Managua'))
mapa_nic <- mapa_nic %>% dplyr::filter(NAME_1 != 'Lago Nicaragua')
mapa_nic <- rbind(mapa_nic, new_mng)
mapa_nic <- mapa_nic %>% rename(Departamento = NAME_1)

y <- ggplot(mapa_nic, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "blue")
print(y)

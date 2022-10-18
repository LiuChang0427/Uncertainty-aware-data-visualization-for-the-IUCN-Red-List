library(tidyverse)
library(sf)
library(dplyr)
library(leaflet)
library(RColorBrewer) 
library(ggmap)
library(rworldmap)
library(graphics)
library(sp)
library(reshape2)

Angelfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/ANGELFISH/ANGELFISH.shp")
Blennies_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/BLENNIES/BLENNIES.shp")
Bonefishes_Tarpons_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/BONEFISH_TARPONS/BONEFISH_TARPONS.shp")
Butterflyfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/BUTTERFLYFISH/BUTTERFLYFISH.shp") 
  
Sharksrays_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/SHARKS_RAYS_CHIMAERAS/SHARKS_RAYS_CHIMAERAS.shp")

Clupeiformes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/CLUPEIFORMES/CLUPEIFORMES.shp")
Groupers_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/GROUPERS/GROUPERS.shp")
Hagfishes_spatial<- st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/HAGFISH/HAGFISH.shp")
Pufferfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/PUFFERFISH/PUFFERFISH.shp")

SeabreamsPorgies_Picarels_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/SEABREAMS_PORGIES_PICARELS/SEABREAMS_PORGIES_PICARELS.shp")
SurgeonfishesTangs_Unicornfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/SURGEONFISHES_TANGS_UNICORNFISHES/SURGEONFISHES_TANGS_UNICORNFISHES.shp")
Syngnathiform_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/SYNGNATHIFORM_FISHES/SYNGNATHIFORM_FISHES.shp")
TunasBillfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/TUNAS_BILLFISHES/TUNAS_BILLFISHES.shp")
WrassesParrotfishes_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/WRASSES_PARROTFISHES/WRASSES_PARROTFISHES.shp")

fishes_spatial<-rbind(Angelfishes_spatial,Blennies_spatial,Bonefishes_Tarpons_spatial,Butterflyfishes_spatial,Sharksrays_spatial,
                      Clupeiformes_spatial,Groupers_spatial,Hagfishes_spatial,Pufferfishes_spatial,
                      SeabreamsPorgies_Picarels_spatial,SurgeonfishesTangs_Unicornfishes_spatial,Syngnathiform_spatial,WrassesParrotfishes_spatial)


MarineFish_01<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/MARINEFISH/MARINEFISH_PART1.shp")
MarineFish_02<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/MARINEFISH/MARINEFISH_PART2.shp")
MarineFish_03<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/Fishes鱼/MARINEFISH/MARINEFISH_PART3.shp")
MarineFish_spatial<-rbind(MarineFish_01,MarineFish_02,MarineFish_03)

reptiles_spatial<-st_read("/Users/mitaonaicha/Desktop/IUCN_空间数据SpatialData/IUCN/plotData/REPTILES_all/REPTILES.shp")


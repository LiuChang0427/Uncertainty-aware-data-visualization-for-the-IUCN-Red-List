library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(ggmap)
library(dplyr)
library(leaflet)
library(DataEditR)
library(shinyWidgets)
library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(DT)
library(ggfittext)
library(ggrepel)
library(RColorBrewer) 
library(rworldmap)
library(graphics)
library(sp)
library(sf)
library(ggthemes)
library(rworldmap)
library(ggrepel)
library(rworldmap)
library(maptools)
library(scales)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)

# 01 红色指数地图
RLIndex<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/redlist_index_data/20221001redlist-data.csv",sep = ",",header = T)
names(RLIndex)[1]
names(RLIndex)[names(RLIndex)=="Entity"]<-"region"
names(RLIndex)[4]
names(RLIndex)[names(RLIndex)=="X15.5.1...Red.List.Index...ER_RSK_LST"]<-"RL_Index"

## 1.Get API Key
googleMap_api<-"AIzaSyDmxGxDex0_ehZcglIXg3ONOfZtu9Tslcc"

## 2. Register key
register_google(key=googleMap_api)

##3.Generate map(test)//不是regional数据？？？
mapdata<-map_data("world")

# 02 气泡地图和后面03使用相同的数据 


#03 global/regional plot data
ShowAll<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/Table\ 5汇总数据\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ show\ all.csv",sep=",",header=T)
Antarctic<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-1南极洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Antarctic.csv",sep=",",header=T)
Caribbean_Islands<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-2加勒比群岛\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Caribbean\ Islands.csv",sep=",",header=T)
East_Asia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-3东亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ East\ Asia.csv",sep=",",header=T)

Europe<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-4欧洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Europe.csv",sep=",",header=T)
Mesoamerica<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-5中美洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Mesoamerica.csv",sep=",",header=T)
North_Africa<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-6北非\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ Africa.csv",sep=",",header=T)
North_America<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-7北美\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ America.csv",sep=",",header=T)

North_Asia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-8北亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ Asia.csv",sep=",",header=T)
Oceania<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-9大洋洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Oceania.csv",sep=",",header=T)
South_America<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-10南美\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ South\ America.csv",sep=",",header=T)
SouthandSoutheastAsia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-11南亚东南亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ South\ and\ Southeast\ Asia.csv",sep=",",header=T)
Saharan_Africa<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-12撒哈拉以南非洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Sub-Saharan\ Africa.csv",sep=",",header=T)
WestandCentralAsia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-13西亚中亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ West\ and\ Central\ Asia.csv",sep=",",header=T)


#regional category data
ShowAll_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_00show\ all_Animal\ species\ kingdom\ Animalia\ by\ country.csv",sep=",",header=T)
Antarctic_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_01Antarctic_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Caribbean_Islands_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_02Caribbean\ Islands_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
East_Asia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_03East\ Asia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)

Europe_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_04Europe_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Mesoamerica_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_05Mesoamerica_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
North_Africa_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_06North\ Africa_Animal\ species\ _kingdom\ Animalia_by\ country.csv",sep=",",header=T)

#报错3个,不影响数据读入
North_America_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_07NorthAmerica_Animal\ species_kingdomAnimalia_by\ country.csv",sep=",",header=T)
North_Asia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_08NorthAsia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Oceania_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_09Oceania_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)

South_America_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_10SouthAmerica_Animalspecies_kingdomAnimalia_bycountry.csv",sep=",",header=T)
SouthandSoutheastAsia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_11South\ AndSoutheastAsia_Animalspecies_kingdomAnimalia_bycountry.csv",sep=",",header=T)
Saharan_Africa_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_12South_SaharanAfrica_Animalspecies_kingdomAnimalia_by\ country.csv",sep=",",header=T)
WestandCentralAsia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_13West\ And\ Central\ Asia_Animal\ species\ _kingdom\ Animalia_by\ country.csv",sep=",",header=T)


##04 Usecase Data
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)
Amphibian<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a_2_Amphibian_Animal_species_by_class_order.csv",sep=",",header=T)
Reptilian<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-31reptilia\ Animal\ species\ by\ class\ and\ order\ -\ REPTILIA.csv",sep=",",header=T)
Insecta<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-22insecta_Animal\ species\ by\ class\ and\ order\ -\ INSECTA.csv",sep=",",header=T)
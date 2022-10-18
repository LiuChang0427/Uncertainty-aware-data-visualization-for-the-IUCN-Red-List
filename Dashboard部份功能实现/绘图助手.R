library(githubinstall)
library(devtools)
library(rJava)
library(usethis)
library(esquisse)
library(ggplot2)

devtools::install_github("calligross/ggthemeassist")
devtools::install_github("rstudio/miniUI")
devtools::install_github("Appsilon/shiny.semantic")

mammalia<- read.csv(file ="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv" ,header = T,sep=",",row.names = 1)
mammalia[,1]
esquisse::esquisser()





library(ggplot2)

ggplot(mammalia) +
 aes(x = Name, y = Total, fill = DD) +
 geom_tile(size = 1.2) +
 scale_fill_gradient() +
 labs(x = "Species_name", y = "DD vs Total") +
 coord_flip() +
 hrbrthemes::theme_ipsum_tw()

ggplot(mammalia) +
 aes(x = Name, y = Total, fill = DD) +
 geom_tile(size = 1.2) +
 scale_fill_gradient() +
 labs(x = "Species_name", y = "DD vs Total") +
 coord_flip() +
 hrbrthemes::theme_ipsum_tw()
library(ggplot2)

ggplot(dataAnimala) +
 aes(x = Subtotal..threatened.spp.., y = Name, fill = CR, colour = CR) +
 geom_tile(size = 1.2) +
 scale_fill_gradient() +
 scale_color_gradient() +
 labs(x = "Threatened Categories(CR)", y = "Species_name") +
 theme_minimal()

ggplot(dataAnimala) +
 aes(x = Subtotal..threatened.spp.., y = Name, fill = CR, colour = CR) +
 geom_tile(size = 1.2) +
 scale_fill_gradient() +
 scale_color_gradient() +
 labs(x = "Threatened Categories(CR)", y = "Species_name") +
 theme_minimal()
library(ggplot2)

ggplot(dataAnimala) +
 aes(x = Name, y = EW, colour = EW) +
 geom_jitter(size = 1.5) +
 scale_color_gradient() +
 labs(y = "Species_name") +
 coord_flip() +
 theme_minimal()

ggplot(dataAnimala) +
 aes(x = Name, y = EW, colour = EW) +
 geom_jitter(size = 2) +
 scale_color_gradient() +
 labs(y = "Species_name") +
 coord_flip() +
 theme_minimal()



esquisse::esquisser()

library(ggplot2)

ggplot(dataAnimala) +
 aes(x = Name, y = NT.or.LR.nt, fill = NT.or.LR.nt, colour = NT.or.LR.nt, group = NT.or.LR.nt) +
 geom_col() +
 scale_fill_gradient() +
 scale_color_gradient() +
 labs(x = "Species_name", y = "NT", 
 fill = "NT", color = "NT") +
 theme_minimal() +
 theme(legend.position = "bottom")

ggplot(dataAnimala) +
 aes(x = Name, y = NT.or.LR.nt, fill = NT.or.LR.nt, colour = NT.or.LR.nt, group = NT.or.LR.nt) +
 geom_col() +
 scale_fill_gradient() +
 scale_color_gradient() +
 labs(x = "Species_name", y = "NT", 
 fill = "NT", color = "NT") +
 theme_minimal() +
 theme(legend.position = "bottom")


## 堆叠条形图
library(ggplot2)


ggplot(cabbage_exp,aes(Date,Weight,fill=Cultivar))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "Pastel1")


mammalia<- read.csv(file ="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv" ,header = T,sep=",")
mammalia
x   <- data()[mammalia[,1]] 
View(x)


library(ggplot2)

ggplot(east_Asia) +
 aes(x = Name, y = Total, fill = Mammals,Birds,Reptiles,Amphibians,Fishes) +
 geom_tile(size = 1.5) +
 scale_fill_viridis_c(option = "viridis", 
 direction = 1) +
 theme_minimal()

east_Asia<-read.csv("/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-3东亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ East\ Asia.csv",header = T,sep=",")
east_Asia
ggplot(east_Asia,aes(x = Total, y=Mammals), fill =Name)+
  geom_bar(position = "stack")










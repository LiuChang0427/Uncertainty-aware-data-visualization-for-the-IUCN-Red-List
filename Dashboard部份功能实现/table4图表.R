library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(echarts4r)
library(DataEditR)
library(echarts4r)


amphibians_species_by_Class_Order<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a_2_Amphibian_Animal_species_by_class_order.csv",sep=",",header=T)

#数据处理
data_speciesbyClassOrder<-melt(data =amphibians_species_by_Class_Order,
                               id.vars=c("Name","Total"),
                               measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"),
                               variable.name = "RL_Category",
                               value.name = "Amount"
                               )

data_speciesbyClassOrder$RL_Category<-factor(data_speciesbyClassOrder$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
data_speciesbyClassOrder$Total<-gsub(",","",data_speciesbyClassOrder$Total)%>% as.numeric()
Total

data_speciesbyClassOrder$Amount<-gsub(",","",data_speciesbyClassOrder$Amount)%>% as.numeric()
View(data_speciesbyClassOrder)

map<-ggplot(data_speciesbyClassOrder) +
  aes(x = Name,y=Amount, fill =RL_Category ) +
  geom_bar(stat = "identity",position=position_stack(vjust=0.5),width = 0.4) +
  scale_fill_manual(
    values = c(EX = "#440154",
               EW = "#462C7A",
               CR = "#3A518A",
               EN = "#2C718E",
               VU = "#22908B",
               NT.or.LR.nt = "#ABDB31",
               LC.or.LR.lc ="#5FC760",
               DD = "#A6A5AA",
               Total = "#FDE725")
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1))+
  coord_flip()
ggplotly(map)



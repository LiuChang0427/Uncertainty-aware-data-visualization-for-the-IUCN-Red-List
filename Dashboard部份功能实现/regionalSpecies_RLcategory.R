library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(DataEditR)

##output$regionSpecies_Plot <- renderPlot({})
  eastAsia_species<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_03East\ Asia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
  data_species_byregion<-melt(data=eastAsia_species,
                        id.vars=c("Name","Total"),
                        measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"),
                        variable.name = "RL_Category",
                        value.name = "Amount")
  
  data_species_byregion$RL_Category<-factor(data_species_byregion$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
  data_species_byregion$Total<-gsub(",","",data_species_byregion$Total)%>% as.numeric()
  data_species_byregion$Amount<-gsub(",","",data_species_byregion$Amount) %>% as.numeric()
  View(data_species_byregion)
  
map<-ggplot(data_species_byregion) +
    aes(x = Name, y = Amount, fill = RL_Category) +
    geom_col(width = 0.5) +
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
    labs(x = "Countries", y = "Amount") +
    theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1,size=6))+
    theme_minimal() +
    theme(legend.position = "top")+
    coord_flip()
  ggplotly(map)
  
  # Uncertainty
  data_uncertainty_byregion<-melt(data=eastAsia_species,
                              id.vars="Name",
                              measure.vars = c("DD","Total"),
                              variable.name = "Uncertainty",
                              value.name = "Amount")
  #charater转成数字
  data_uncertainty_byregion$Amount<-gsub(",","",data_uncertainty_byregion$Amount) %>% as.numeric()
  #绘图：
map3<-ggplot(data_uncertainty_byregion) +
    aes(x = Name, y = Amount, fill = Uncertainty) +
    geom_col() +
    scale_fill_manual(
      values = c(DD = "#A6A5AA",Total = "#FDE725")
    )+
    labs(x = "Countries' Name",fill = "Uncertainty") +
    scale_x_discrete(labels=function(x)str_replace_all(x,"(.{5})","\\1\n"))
    theme_minimal()
 ggplotly(map3)


library(reshape2)
library(tidyverse)
library(dplyr)
library(ggfittext)
library(ggplot2)
library(ggrepel)
library(DataEditR)
library(data.table)

# Uncertainty in Biodiversity + error Bar
#csv文件读取
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

#数据处理
Mammalia$Total<-gsub(",","",Mammalia$Total) %>% as.numeric()
Mammalia$DD<-gsub(",","",Mammalia$DD) %>% as.numeric()
Total_dd<-(Mammalia$Total-Mammalia$DD)

total_dataNew <-mutate(Mammalia,Total_dd,NE=6577-5969,total_inDB=6577)
View(total_dataNew)

length_data<-length(total_dataNew$NE)%>% as.numeric()-1
length_data

total_dataNew[c(1:length_data),"NE"]<-0
total_dataNew[c(1:length_data),"total_inDB"]<-0

View(total_dataNew)

#长数据转换为宽数据
uncertainty<-reshape2::melt(data=total_dataNew,
                 id.vars=c("Name","Total","total_inDB"),
                 measure.vars = c("DD","Total_dd","NE"),
                 variable.name = "Category",
                 value.name = "Amount")
uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()


uncertain_summary<-aggregate(Amount~Name,
                             data = uncertainty,
                             function(x)c(mean=mean(x),
                                          sd=sd(x),
                                          se=sd(x)/sqrt(length(x))
                                          
                             ))
uncertain_summary<-data.frame(Name=uncertain_summary[,1],uncertain_summary$Amount)

# 4.合并数据
mapdata_new<-left_join(total_dataNew,uncertain_summary,by="Name")


#5.再次长数据转化为宽数据
uncertainty<-reshape2::melt(data=mapdata_new,
                  id.vars=c("Name","mean","sd","se","total_inDB","Total"),
                  measure.vars = c("Total_dd","DD","NE"),
                  variable.name = "Category",
                  value.name = "Amount")
uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
uncertainty$Category<-factor(uncertainty$Category,ordered = T,levels = c("NE","DD","Total_dd"))


#基本图形
errorbar_data<- data.table(uncertainty)[, list(Amount = sum(Total), se_m = mean(se), Or = "Total_dd"), by = c("Name", "Category")] 
errorbar_data

barplot_error<-ggplot(uncertainty) +
  aes(x = Name, y = Amount, fill = Category) +
  geom_col(width=0.6,position=position_stack(vjust=1)) +
  labs(
    subtitle = "Uncertainty in Biodiversity",
    fill = "Uncertainty")+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=6))+
  coord_flip()+
  geom_errorbar(data=errorbar_data,
                inherit.aes = FALSE,
                width=0.4,
                aes(x=Name,ymax = Amount+se_m, ymin=Amount-se_m), 
                position=position_dodge(0.9), colour="red")+
  scale_fill_manual(
    values = c(NE = "#440154",
               DD = "#22908B",
               Total_dd = "#FDE725")
  )  +
  theme_minimal()

print(barplot_error)



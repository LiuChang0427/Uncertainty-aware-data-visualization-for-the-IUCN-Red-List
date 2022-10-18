library(reshape2)
library(tidyverse)
library(dplyr)
library(ggfittext)
library(ggplot2)
library(ggrepel)
library(DataEditR)
library(data.table)

#1.读取数据
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

#2. 添加NE数据和total_inDB
total_dataNew <- mutate(Mammalia,NE=6577-5969,total_inDB=6577)

total_dataNew[c(1:27),"NE"]<-0
total_dataNew[c(1:27),"total_inDB"]<-0
View(total_dataNew)

#3.宽数据转化为长数据，并计算平均数 标准差 标准误
uncertainty<-melt(data=total_dataNew,
                  id.vars="Name",
                  measure.vars = c("DD","Total","NE"),
                  variable.name = "Category",
                  value.name = "Amount")
uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
uncertainty$Category<-factor(uncertainty$Category,ordered = T,levels = c("NE","DD","Total"))

uncertain_summary<-aggregate(Amount~Name,
                             data = uncertainty,
                             function(x)c(mean=mean(x),
                                          sd=sd(x),
                                          se=sd(x)/sqrt(length(x))
                                          
                             ))
uncertain_summary<-data.frame(Name=uncertain_summary[,1],uncertain_summary$Amount)
View(uncertain_summary)
# 4.合并数据
mapdata_new<-left_join(total_dataNew,uncertain_summary,by="Name")
View(mapdata_new)

#5.再次长数据转化为宽数据
uncertainty<-melt(data=mapdata_new,
                  id.vars=c("Name","mean","sd","se","total_inDB"),
                  measure.vars = c("Total","NE"),
                  variable.name = "Category",
                  value.name = "Amount")
uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
uncertainty$Category<-factor(uncertainty$Category,ordered = T,levels = c("NE","DD","Total"))
View(uncertainty)


ggplot(uncertainty) +
  aes(x = Name, y = Amount, fill = Category) +
  geom_col() +
  scale_fill_manual(
    values = c(NE = "#440154",
               DD = "#22908B",
               Total = "#FDE725")
  ) +
  coord_flip()+
  labs(fill = "Uncertainty Category") +
  theme_minimal()+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=6))+
  theme(axis.text.x=element_text(size=rel(0.6), angle=40))


#新方案尝试
test01<- data.table(uncertainty)[, list(Amount = sum(Amount), se = mean(se)), by = c("Name", "Category")] 

ggplot(uncertainty,aes(x=factor(Name),y=Amount,fill=factor(Category))) +
  geom_bar(stat="identity")+
  geom_errorbar(data = test01, aes(ymax = Amount + se, ymin= Amount -se), position="dodge", colour="red", width=.65)+
  scale_fill_viridis_d(option = "viridis", direction = 1) 



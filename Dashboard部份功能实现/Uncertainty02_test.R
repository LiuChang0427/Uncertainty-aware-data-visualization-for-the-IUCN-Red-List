library(reshape2)
library(tidyverse)
library(dplyr)
library(ggfittext)
library(ggplot2)
library(ggrepel)

# Uncertainty in Biodiversity + error Bar
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

#数据处理
total_dataNew <- mutate(Mammalia,NE=6577-5969,total_inDB=6577)
View(total_dataNew)

total_dataNew[c(1:27),"NE"]<-0
total_dataNew[c(1:27),"total_inDB"]<-0
View(total_dataNew)

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
##4.合并数据
mapdata_new<-left_join(uncertainty,uncertain_summary,by="Name")


##5. 基本图形+误差条

#5.1 柱状图+errorbar
ggplot(uncertain_summary) +
  aes(x = Name, fill = Name, y = mean) +
  geom_col() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=0.25,size=1,
                position=position_dodge(0.9),alpha=0.6)+
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  labs(fill = "Red List Category") +
  theme_minimal()+
  coord_flip()+
  guides(fill=F)+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=6))+
  theme(axis.text.x=element_text(size=rel(0.6), angle=40))









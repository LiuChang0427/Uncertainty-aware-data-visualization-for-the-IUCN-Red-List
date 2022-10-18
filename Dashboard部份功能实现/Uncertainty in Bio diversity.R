library(reshape2)
library(tidyverse)
library(dplyr)
library(ggfittext)
library(ggplot2)
library(ggrepel)
library(plotly)

# Uncertainty in Biodiversity + error Bar
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)
View(Mammalia)


#数据处理
total_dataNew <- mutate(Mammalia,NE=6577-5969,total_inDB=6577)

total_dataNew[c(1:27),"NE"]<-0
total_dataNew[c(1:27),"total_inDB"]<-0
View(total_dataNew)


total<-melt(data=total_dataNew,
            id.vars="Name",
            measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD","Total","NE","total_inDB"),
            variable.name = "Category",
            value.name = "Amount")
View(total)

total$Amount<-gsub(",","",total$Amount) %>% as.numeric()

#计算均值、标准差,有问题
data_summary<-aggregate(Amount~Category,
                        data = total,
                        function(x)c(mean=mean(x),
                                     sd=sd(x),
                                     se=sd(x)/sqrt(length(x))
                          
                        ))

data_summary<-data.frame(group=data_summary[,1],data_summary$Amount)
View(data_summary)
write.table(data_summary,"data_summary.csv",row.names=FALSE,col.names=TRUE,sep=",")

data_summary2<-aggregate(Amount~Name,
                        data = total,
                        function(x)c(mean=mean(x),
                                     sd=sd(x),
                                     se=sd(x)/sqrt(length(x))
                                     
                        ))
data_summary2<-data.frame(group=data_summary[,1],data_summary$Amount)




# 基本图形
b_plot<-barplot(data_summary$mean~group,
                data_summary,
                ylim=c(0,500))
arrows(x0=b_plot,
       y0=data_summary$mean+data_summary$se,
       y1=data_summary$mean-data_summary$se,
       angle = 90,
       code=3,
       length=0.1)

# ggplot2图形,有问题
ggplot(data_summary) +
  aes(x = group, fill = group, weight = mean) +
  geom_bar() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=0.25,size=1,
                position=position_dodge(0.9),alpha=0.6)+
  scale_fill_manual(
    values = c(EX = "#440154",
               EW = "#462C7A",
               CR = "#3A518A",
               EN = "#2C718E",
               VU = "#22908B",
               NT.or.LR.nt = "#ABDB31",
               LC.or.LR.lc ="#5FC760",
               DD = "#A6A5AA",
               NE="#807C93",
               Total ="#FDE725",
               total_inDB="red")
  )+
  labs(fill = "Red List Category") +
  theme_minimal()




uncertainty<-melt(data=total_dataNew,
            id.vars="Name",
            measure.vars = c("DD","Total","NE"),
            variable.name = "Category",
            value.name = "Amount")
uncertainty$Amount<-gsub(",","",uncertainty$Amount) %>% as.numeric()
uncertainty$Category<-factor(uncertainty$Category,ordered = T,levels = c("NE","DD","Total"))

data_summary2<-aggregate(Amount~Name,
                         data = uncertainty,
                         function(x)c(mean=mean(x),
                                      sd=sd(x),
                                      se=sd(x)/sqrt(length(x))
                                      
                         ))
data_summary2<-data.frame(group=data_summary2[,1],data_summary2$Amount)
ggplot(data_summary2) +
  aes(x = group, fill = group, weight = mean) +
  geom_bar() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=0.25,size=1,
                position=position_dodge(0.9),alpha=0.6)+
  scale_fill_hue(direction = 1) +
  labs(fill = "Red List Category") +
  theme_minimal()+
  coord_flip()+
  guides(fill=F)

errorbar_plot<-ggplot(data_summary2) +
  aes(x = group, fill = group, weight = mean) +
  geom_bar() +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),
                width=0.25,size=1,
                position=position_dodge(0.9),alpha=0.6)+
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  labs(fill = "Red List Category") +
  theme_minimal()+
  coord_flip()







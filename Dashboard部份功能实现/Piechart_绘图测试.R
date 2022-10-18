# 饼图绘图测试
library(reshape2)
library(tidyverse)
library(ggfittext)
library(ggplot2)
library(ggrepel)

Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

total_data<-Mammalia[28,]
View(total_data)

total_data2<-tail(Mammalia,1)

total_inDB<-6577
class(total_inDB)
total<-gsub(",","",total_data2$Total) %>% as.numeric()
total
NE<-(total_inDB-total)

total_dataNew <- mutate(total_data,NE,total_inDB)
View(total_dataNew)

total<-melt(data=total_dataNew,
            id.vars="Name",
            measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD","NE"),
            variable.name = "parts&whole",
            value.name = "Amount")
total$Amount<-gsub(",","",total$Amount) %>% as.numeric()
View(total)
total.data <- total %>%
  arrange(desc(`parts&whole`)) %>%
  mutate(percentage = Amount /6577)

ggplot(data=total.data)+
  aes(x=2, y = Amount, fill = `parts&whole`) +
  geom_bar(stat="identity",width=1,position = 'stack',alpha=0.95)+
  coord_polar(theta ="y",start = 0)+
  scale_fill_manual(
    values = c(EX = "#440154",
               EW = "#462C7A",
               CR = "#3A518A",
               EN = "#2C718E",
               VU = "#22908B",
               NT.or.LR.nt = "#ABDB31",
               LC.or.LR.lc ="#5FC760",
               DD = "#A6A5AA",
               NE="#807C93")
  )+
  theme_void()+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5))+
  geom_label_repel(data = total.data,
                   aes(label = str_c(Amount,"\n",scales::percent(percentage))),
                   size = 2.5, show.legend = FALSE,segment.size = 0.5,position = position_stack(vjust = 0.5),vjust="center",hjust="center") +
  guides(fill = guide_legend(title = "Red List Category"))+
  xlim(0.5, 2.5)


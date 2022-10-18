library(reshape2)
library(tidyverse)
library(ggfittext)
library(ggplot2)
library(ggrepel)
library(plotly)
library(shinyWidgets)
library(ggthemes)
library(scales)
library(visdat)

scale_fill_tableau(
  palette = "Tableau 10",
  type = "regular",
  direction = -1
)

palette1<-scale_fill_tableau(
  palette = "Tableau 10",
  type = "regular",
  values = c(EX = "#a87c9f",
             EW = "#462C7A",
             CR = "#d1615d",
             EN = "#2C718E",
             VU = "#e7ca60",
             NT.or.LR.nt = "#85b6b2",
             LC.or.LR.lc ="#6a9f58",
             DD = "#b8b0ac",
             Total = "#85b6b2")
)

tableau10<-c(
    blue="#5778a4",
    orange="#e49444",
    red="#d1615d",
    teal="#85b6b2",
    green="#6a9f58",
    yellow="#e7ca60",
    purple="#a87c9f",
    pink="#f1a2a9",
    brown="#967662",
    grey="#b8b0ac"
)
tableau10 <- define_palette(swatch = tableau10,
                            gradient = c(EX = "#a87c9f",
                                         EW = "#462C7A",
                                         CR = "#d1615d",
                                         EN = "#2C718E",
                                         VU = "#e7ca60",
                                         NT.or.LR.nt = "#85b6b2",
                                         LC.or.LR.lc ="#6a9f58",
                                         DD = "#b8b0ac",
                                         Total = "#85b6b2")
)



# uncertainty Vis
Mammalia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

## 1. 7 Catgories
Mammlia_byRLCategory<-melt(data=Mammalia,
                           id.vars=c("Name","Total"),
                           measure.vars = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"),
                           variable.name = "RL_Category",
                           value.name = "Amount")
Mammlia_byRLCategory$RL_Category<-factor(Mammlia_byRLCategory$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
Mammlia_byRLCategory$Total<-gsub(",","",Mammlia_byRLCategory$Total)%>% as.numeric()
Mammlia_byRLCategory$Amount<-gsub(",","",Mammlia_byRLCategory$Amount) %>% as.numeric()
View(Mammlia_byRLCategory)

map_mammalia<-ggplot(Mammlia_byRLCategory) +
  aes(x = reorder(Name,-Amount), y = Amount, fill = RL_Category) +
  geom_col() +
  scale_fill_manual(
    values = c(EX = "#462C7A",
               EW = "#a87c9f",
               CR = "#d1615d",
               EN = "#e7ca60",
               VU = "#2C718E",
               NT.or.LR.nt = "#85b6b2",
               LC.or.LR.lc ="#6a9f58",
               DD = "#b8b0ac",
               Total = "#967662")
  )+
  labs(x = "Name", y = "Amount") +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=4),
        axis.text.y = element_text(size=6))+
  theme(axis.text.x=element_text(size=rel(0.6), angle=40))

ggplotly(map_mammalia)

##饼状图
total_data<-Mammalia[28,]

total_dataNew <- mutate(total_data,NE=6577-5969,total_inDB=6577)


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

##3.DD vs 7 Category
DD <-melt(data=Mammalia,
          id.vars="Name",
          measure.vars = c("DD","Total"),
          variable.name = "Data_deficiency",
          value.name = "Amount")
#charater转成数字
DD$Amount<-gsub(",","",DD$Amount) %>% as.numeric()

#绘图
DD_plot<-ggplot(DD) +
  aes(x=reorder(Name,-Amount), y = Amount, fill = Data_deficiency) +
  geom_col(position="fill") +
  scale_fill_manual(
    values = c(DD = "#A6A5AA",Total = "#FDE725")
  )+ 
  labs(x="Name",y = "Percentage",fill = "Data Deficiency") +
  coord_flip() +
  theme_minimal()+
  theme(legend.title = element_text(size = 6), 
        legend.text = element_text(size = 5),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))+
  theme(axis.text.x=element_text(size=rel(0.6), angle=40))+
  
ggplotly(DD_plot)

#4.Uncertainty in Biodiversity


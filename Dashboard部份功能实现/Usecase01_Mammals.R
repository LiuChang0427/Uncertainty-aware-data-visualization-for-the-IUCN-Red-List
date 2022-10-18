library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(modelStudio)
library(tidymodels)
library(xgboost)
library(devtools)
library(rJava)
library(usethis)

devtools::install_github('BirdLifeInternational/rli-codes')
devtools::install_github("owid/owid-grapher")

MAMMALIA_byClassandOrder<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 4a\ Animal\ species\ by\ class\ and\ order/4a-24MAMMALIA\ Animal\ species\ by\ class\ and\ order.csv",sep=",",header=T)

## 图1 7 Category
Mammlia_byRLCategory<-melt(data=MAMMALIA_byClassandOrder,
                           id.vars=c("Name","Total"),
                           measure.vars = c("EX","EW","CR","EN","VU","NT","LC","DD"),
                           variable.name = "RL_Category",
                           value.name = "Amount")

Mammlia_byRLCategory$RL_Category<-factor(Mammlia_byRLCategory$RL_Category,ordered = T,levels = c("EX","EW","CR","EN","VU","NT.or.LR.nt","LC.or.LR.lc","DD"))
Mammlia_byRLCategory$Total<-gsub(",","",Mammlia_byRLCategory$Total)%>% as.numeric()
Mammlia_byRLCategory$Amount<-gsub(",","",Mammlia_byRLCategory$Amount) %>% as.numeric()
View(Mammlia_byRLCategory)

map_mammalia<-ggplot(Mammlia_byRLCategory) +
  aes(x = Name, y = Amount, fill = RL_Category) +
  geom_col() +
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
  labs(x = "Name", y = "Amount") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top")
ggplotly(map_mammalia)


## 图2 DD VS Total
Mammlia_byDD<-melt(data=MAMMALIA_byClassandOrder,
                           id.vars=c("Name"),
                           measure.vars = c("DD","Total"),
                           variable.name = "DDandTotal",
                           value.name = "Amount")
#charater转成数字
Mammlia_byDD$Amount<-gsub(",","",Mammlia_byDD$Amount) %>% as.numeric()
# 按从大到小顺序排列
map_DD<-ggplot(Mammlia_byDD) +
  aes(x = reorder(Name,-Amount), y = Amount, fill = DDandTotal) +
  geom_col() +
  scale_fill_manual(
    values = c(DD = "#A6A5AA",
               Total = "#FDE725")
  ) +
  labs(x = "Name", y = "Amount",fill = "DD VS Total") +
  coord_flip() +
  theme_minimal()
ggplotly(map_DD)

# 按在数据集中出现的顺序排列
map_DD2<-ggplot(Mammlia_byDD) +
  aes(x = Name, y = Amount, fill = DDandTotal) +
  geom_col() +
  scale_fill_manual(
    values = c(DD = "#A6A5AA",
               Total = "#FDE725")
  ) +
  labs(x = "Name", y = "Amount",fill = "DD VS Total") +
  coord_flip() +
  theme_minimal()+
  scale_x_discrete(limits=c('AFROSORICIDA','CARNIVORA','CETARTIODACTYLA','CHIROPTERA','CINGULATA',
                            'DASYUROMORPHIA','DERMOPTERA','DIDELPHIMORPHIA','DIPROTODONTIA','EULIPOTYPHLA','HYRACOIDEA',
                            'LAGOMORPHA','MACROSCELIDEA','MICROBIOTHERIA','MONOTREMATA','NOTORYCTEMORPHIA','PAUCITUBERCULATA',
                            'PERAMELEMORPHIA','PERISSODACTYLA','PHOLIDOTA','PILOSA','PRIMATES','PROBOSCIDEA',
                            'RODENTIA','SCANDENTIA','SIRENIA','TUBULIDENTATA','Total'))
ggplotly(map_DD2)


# modelStudio
data_uncertaity<-MAMMALIA_byClassandOrder %>%
  select(Name,EX,EW,CR,EN,VU,NT.or.LR.nt,LC.or.LR.lc,DD,Total)
data_uncertaity$Total<-gsub(",","",data_uncertaity$Total)%>% as.numeric()

fit_xgboost<-boost_tree(learn_rate=0.3) %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit(Total ~.,data=data_uncertaity)
fit_xgboost 

explainer<-DALEX::explain(
  model=fit_xgboost,
  data=data_uncertaity,
  y=data_uncertaity$Total,
  label="Uncertainty"
)  
modelStudio::modelStudio(explainer)

# Uncertainty in Biodiversity + error Bar









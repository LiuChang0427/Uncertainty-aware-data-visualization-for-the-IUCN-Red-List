library(ggThemeAssist)
library(reshape2)
library(DataEditR)

eastAsia<-read.csv(file = "/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-3东亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ East\ Asia.csv",sep=",",header = T)
eastAsia
x    <- length(eastAsia[,1])
x
class(eastAsia$Name)
class(eastAsia$Mammals)
eastAsia$Mammals<-as.numeric(eastAsia$Mammals)
class(eastAsia$Mammals)

eastAsia_new2<-data_edit(eastAsia)
View(eastAsia_new2)

library(ggplot2)
ggplot(eastAsia_new2) +
 aes(x = V1, fill = Name, weight = MClass) +
 geom_bar() +
 scale_fill_hue(direction = 4) +
 labs(x = "Major Group",y="", fill = "Countries' Name") +
 coord_flip() +
 theme_minimal() +
 theme(legend.position = "top")

library(ggplot2)

ggplot(eastAsia_new2) +
 aes(x = V1, y = Class, fill = Name) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 labs(x = "Class", y = "Amount") +
 coord_flip() +
 theme_minimal() +
 theme(legend.position = "top")
dev.off()

#调色
ggplot(eastAsia_new2) +
  aes(x = V1, fill = Name, weight = Class) +
  geom_bar() +
  scale_fill_brewer(palette="RdBu") +
  labs(x = "Major Group",y="", fill = "Countries' Name") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top")


eastAsia_new<-melt(data=eastAsia,
                   id.vars = c("Name","Plants.","Fungi.","Chromists."),
                   measure.vars = c("Mammals","Birds","Reptiles.","Amphibians","Fishes.","Molluscs.","Other.Inverts.","Total"),
                   variable.name ="g.eastAsia",
                   value.name="Distribution")

 View(eastAsia_new)
 P<-ggplot(eastAsia_new) +
   aes(x = g.eastAsia, fill = Name, weight = as.numeric(Distribution))+
   geom_bar() +
   scale_fill_brewer(palette="RdBu") +
   labs(x = "Major Group",y="", fill = "Countries' Name") +
   coord_flip() +
   theme_minimal() +
   theme(legend.position = "top")
 ggplotly(P)
 
library(ggplot2)
#丑图开会
ggplot(eastAsia_new, aes(x=g.eastAsia,fill=Name,y=Distribution))+
  geom_bar(stat ="identity",position ="stack",width=0.6)+     
  scale_fill_brewer(palette = "Pastel1")+              
  labs(x = "Major Group",y = "Number and Distribution", title = "Threatened species in each major group in EastAsia")+
  theme(
    #标题字体设置
    text=element_text(size=8),
    #设置标题居中hjust = 0.5
    plot.title = element_text(hjust = 0.5,vjust = 0.5))


#东亚物种结构图
eastAsia_Animal_species<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_03East\ Asia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
eastAsia_Animal_species$Subtotal..threatened.spp..
eastAsia_Animal_species_new<-melt(data=eastAsia_Animal_species,
                   id.vars = c("Name"),
                   measure.vars = c("EX","EW","Subtotal..threatened.spp..","NT.or.LR.nt","LC.or.LR.lc","DD","Total"),
                   variable.name ="V1",
                   value.name="Amount")
View(eastAsia_Animal_species_new)

eastAsia_species<-data_edit(eastAsia_Animal_species)

library(ggplot2)
library(plotly)
eastAsia_Animal_species<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_03East\ Asia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
eastAsia_Animal_species$Subtotal..threatened.spp..
eastAsia_Animal_species_new<-melt(data=eastAsia_Animal_species,
                                  id.vars = c("Name"),
                                  measure.vars = as.factor(c("EX","EW","Subtotal..threatened.spp..","NT.or.LR.nt","LC.or.LR.lc","DD")),
                                  variable.name ="V1",
                                  value.name="Amount")

p<-ggplot(eastAsia_Animal_species_new) +
 aes(x = Name, y = Amount, fill = V1) +
 geom_col() +
  scale_fill_brewer(palette="RdBu") +
 labs(x = "Countries", y = "Amount") +
 coord_flip() +
 theme_minimal() +
 theme(legend.position = "top")
ggplotly(p)
View(eastAsia_species)

library(ggplot2)
library(plotly)
p<-ggplot(eastAsia_new) +
  aes(x = specia.eastAsia, fill = Name, weight = Amount)+
  geom_bar() +
  scale_fill_brewer(palette="Set1") +
  labs(x = "Major Group",y="", fill = "Countries' Name") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top")
ggplotly(p)



#Species by country
ShowAll<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/Table\ 5汇总数据\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ show\ all.csv",sep=",",header=T)
Antarctic<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-1南极洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Antarctic.csv",sep=",",header=T)
Caribbean_Islands<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-2加勒比群岛\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Caribbean\ Islands.csv",sep=",",header=T)
East_Asia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-3东亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ East\ Asia.csv",sep=",",header=T)

Europe<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-4欧洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Europe.csv",sep=",",header=T)
Mesoamerica<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-5中美洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Mesoamerica.csv",sep=",",header=T)
North_Africa<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-6北非\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ Africa.csv",sep=",",header=T)
North_America<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-7北美\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ America.csv",sep=",",header=T)

North_Asia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-8北亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ North\ Asia.csv",sep=",",header=T)
Oceania<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-9大洋洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Oceania.csv",sep=",",header=T)
South_America<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-10南美\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ South\ America.csv",sep=",",header=T)
SouthandSoutheastAsia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-11南亚东南亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ South\ and\ Southeast\ Asia.csv",sep=",",header=T)
Saharan_Africa<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-12撒哈拉以南非洲\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ Sub-Saharan\ Africa.csv",sep=",",header=T)
WestandCentralAsia<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 5按国家划分的每个主要分类组的受威胁物种数量/5-13西亚中亚\ \ Threatened\ species\ in\ each\ major\ group\ by\ country\ -\ West\ and\ Central\ Asia.csv",sep=",",header=T)

#Category of RL by country/region
ShowAll_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_00show\ all_Animal\ species\ kingdom\ Animalia\ by\ country.csv",sep=",",header=T)
Antarctic_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_01Antarctic_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Caribbean_Islands_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_02Caribbean\ Islands_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
East_Asia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_03East\ Asia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)

Europe_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_04Europe_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Mesoamerica_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_05Mesoamerica_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
North_Africa_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_06North\ Africa_Animal\ species\ _kingdom\ Animalia_by\ country.csv",sep=",",header=T)

#报错3个,不影响数据读入
North_America_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_07NorthAmerica_Animal\ species_kingdomAnimalia_by\ country.csv",sep=",",header=T)
North_Asia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_08NorthAsia_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)
Oceania_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_09Oceania_Animal\ species_kingdom\ Animalia_by\ country.csv",sep=",",header=T)

South_America_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_10SouthAmerica_Animalspecies_kingdomAnimalia_bycountry.csv",sep=",",header=T)
SouthandSoutheastAsia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_11South\ AndSoutheastAsia_Animalspecies_kingdomAnimalia_bycountry.csv",sep=",",header=T)
Saharan_Africa_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_12South_SaharanAfrica_Animalspecies_kingdomAnimalia_by\ country.csv",sep=",",header=T)
WestandCentralAsia_category<-read.csv(file="/Users/mitaonaicha/Desktop/IUCN物种数据csv/Table\ 6a\ 按国家划分的动物物种王国动物界Animal\ species\ kingdom\ Animalia\ by\ country/Table6a_13West\ And\ Central\ Asia_Animal\ species\ _kingdom\ Animalia_by\ country.csv",sep=",",header=T)


rm(list = ls(all = TRUE)) # Clear all memory
setwd("~/RTSLpaper/data_preprocessing")
############################################
library(dplyr)
library(readxl)

##data can be found at:
#https://population.un.org/wpp/Download/Standard/Interpolated/
#Standard projections, Annual and single age data

male<-read_excel("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx", sheet=1,skip=16)%>%
  filter(`Reference date (as of 1 July)`>=2010)%>%select(`20`, `Reference date (as of 1 July)`, 
                                                         `Region, subregion, country or area *`, `Country code`)
male2<-read_excel("WPP2019_INT_F03_2_POPULATION_BY_AGE_ANNUAL_MALE.xlsx", sheet=2,skip=16)%>%
  filter(`Reference date (as of 1 July)`<=2040 & `Reference date (as of 1 July)`>2020)%>%
  select(`20`, `Reference date (as of 1 July)`, `Region, subregion, country or area *`, `Country code`)

names(male)[1]<-"Nx_chr"
names(male)[2]<-"year_id"
names(male)[4]<-"LocID"

names(male2)[1]<-"Nx_chr"
names(male2)[2]<-"year_id"
names(male2)[4]<-"LocID"

male<-bind_rows(male, male2)

country_ids <- read.csv("Country_groupings_extended.csv", stringsAsFactors=FALSE)
names(country_ids)[3]<-"location"
country_ids<-country_ids%>%select("location", "LocID")      

pop_males<-left_join(country_ids, male, by="LocID")
any(is.na(pop_males))

pop_males$Nx<-as.numeric(pop_males$Nx_chr)

pop_males<-pop_males%>%select(Nx, year_id, location)
pop_males$sex<-"Male"
pop_males$age<-20
pop_males$Nx<-pop_males$Nx*1000

length(pop_males%>%filter(location=="China", sex=="Male")%>%pull(Nx))


female<-read_excel("WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx", sheet=1,skip=16)%>%
  filter(`Reference date (as of 1 July)`>=2010)%>%select(`20`, `Reference date (as of 1 July)`, 
                                                         `Region, subregion, country or area *`, `Country code`)
female2<-read_excel("WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx", sheet=2,skip=16)%>%
  filter(`Reference date (as of 1 July)`<=2040 & `Reference date (as of 1 July)`>2020)%>%
  select(`20`, `Reference date (as of 1 July)`, `Region, subregion, country or area *`, `Country code`)

names(female)[1]<-"Nx_chr"
names(female)[2]<-"year_id"
names(female)[4]<-"LocID"

names(female2)[1]<-"Nx_chr"
names(female2)[2]<-"year_id"
names(female2)[4]<-"LocID"

female<-bind_rows(female, female2)

pop_females<-left_join(country_ids, female, by="LocID")

pop_females$Nx<-as.numeric(pop_females$Nx_chr)

pop_females<-pop_females%>%select(Nx, year_id, location)
pop_females$sex<-"Female"
pop_females$age<-20
pop_females$Nx<-pop_females$Nx*1000
pop<-bind_rows(pop_females, pop_males)

length(pop%>%filter(location=="China", sex=="Male")%>%pull(Nx))
length(pop%>%filter(location=="China", sex=="Female")%>%pull(Nx))

write.csv(pop,"PopulationsAge20_full.csv", row.names = F)

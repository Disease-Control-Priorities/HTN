setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)

load("../model/model_output_newnorm.Rda")

countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%
  filter(super_region == "Sub-Saharan Africa" | 
           location %in% c("Algeria", "Egypt", "Libya", "Morocco",
                           "Sudan", "Tunisia"))%>%
  filter(location!="Global",location!="South Sudan")%>%pull(location)

comb<-progress%>%filter(intervention=='b.a.u', location%in%countrylist)%>%
                  mutate(intervention = "Business as usual")

#WB income groups
groups<-read.csv("super_regions.csv", stringsAsFactors = F)%>%
  filter(location %in% countrylist)%>%
  mutate(super_region = 
         ifelse(location%in%c("Algeria", "Egypt", "Libya", "Morocco",
                              "Sudan", "Tunisia"), "Northern Africa",
         ifelse(location%in%c("Botswana", "Lesotho", "Namibia", 
                              "South Africa", "Swaziland"), "Southern Africa",
         ifelse(location%in%c("Benin", "Burkina Faso", "Cape Verde", 
                              "Cote D'Ivoire", "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                              "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", 
                              "Sierra Leone", "Togo"), "Western Africa",
         ifelse(location %in% c("Angola", "Cameroon", "Central African Republic", 
                              "Chad", "Congo", "Democratic Republic of Congo", "Equatorial Guinea", 
                              "Gabon", "Sao Tome and Principe"), "Central Africa", "Eastern Africa"))))
         )

#########

comb<-left_join(comb, groups, by="location")

CVD<-comb%>%group_by(age, sex, location, super_region, year, intervention)%>%
  filter(age>=30 & age<70)%>%summarise(pop=sum(pop)/4, dead=sum(dead)) #divide pop by 4 to avoid over counting for each cause
CVD$age.group<-NA
CVD$age.group[CVD$age>=30 & CVD$age<35]<-"30-34"
CVD$age.group[CVD$age>=35 & CVD$age<40]<-"35-39"
CVD$age.group[CVD$age>=40 & CVD$age<45]<-"40-44"
CVD$age.group[CVD$age>=45 & CVD$age<50]<-"45-49"
CVD$age.group[CVD$age>=50 & CVD$age<55]<-"50-54"
CVD$age.group[CVD$age>=55 & CVD$age<60]<-"55-59"
CVD$age.group[CVD$age>=60 & CVD$age<65]<-"60-64"
CVD$age.group[CVD$age>=65 & CVD$age<70]<-"65-69"

#by region
WB_50q30<-CVD%>%group_by(age.group,  super_region, year)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(super_region, year)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

ggplot(WB_50q30, aes(x=year, y=x50q30, color=super_region))+
  geom_point()

#write.csv(WB_50q30, "africa_40q30_region.csv")

#by country
country_50q30<-CVD%>%group_by(age.group,location, super_region, year, intervention)%>%summarise(pop=sum(pop), dead=sum(dead))
country_50q30$mx<-country_50q30$dead/country_50q30$pop
any(is.na(country_50q30))

country_50q30<-country_50q30%>%group_by(location,super_region, year, intervention)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

ggplot(country_50q30, 
       aes(x=year, y=x50q30, color=location))+
  geom_point()

#write.csv(country_50q30, "africa_40q30_country.csv")

#by continent
all_50q30<-CVD%>%group_by(age.group, year, intervention)%>%summarise(pop=sum(pop), dead=sum(dead))
all_50q30$mx<-all_50q30$dead/all_50q30$pop

all_50q30<-all_50q30%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))%>%
  mutate(location = "Africa", super_region="Africa")


##add GBD historic data ###
setwd("../data_preprocessing/2. get AARCs")
pop0<-fread("IHME_GBD_2019_POP_SYA_2000_Y2021M01D28.csv")
pop1<-fread("IHME_GBD_2019_POP_SYA_2001_Y2021M01D28.csv")
pop2<-fread("IHME_GBD_2019_POP_SYA_2002_Y2021M01D28.csv")
pop3<-fread("IHME_GBD_2019_POP_SYA_2003_Y2021M01D28.csv")
pop4<-fread("IHME_GBD_2019_POP_SYA_2004_Y2021M01D28.csv")
pop5<-fread("IHME_GBD_2019_POP_SYA_2005_Y2021M01D28.csv")
pop6<-fread("IHME_GBD_2019_POP_SYA_2006_Y2021M01D28.csv")
pop7<-fread("IHME_GBD_2019_POP_SYA_2007_Y2021M01D28.csv")
pop8<-fread("IHME_GBD_2019_POP_SYA_2008_Y2021M01D28.csv")
pop9<-fread("IHME_GBD_2019_POP_SYA_2009_Y2021M01D28.csv")
pop10<-fread("IHME_GBD_2019_POP_SYA_2010_Y2021M01D28.csv")
pop11<-fread("IHME_GBD_2019_POP_SYA_2011_Y2021M01D28.csv")
pop12<-fread("IHME_GBD_2019_POP_SYA_2012_Y2021M01D28.csv")
pop13<-fread("IHME_GBD_2019_POP_SYA_2013_Y2021M01D28.csv")
pop14<-fread("IHME_GBD_2019_POP_SYA_2014_Y2021M01D28.csv")
pop15<-fread("IHME_GBD_2019_POP_SYA_2015_Y2021M01D28.csv")
pop16<-fread("IHME_GBD_2019_POP_SYA_2016_Y2021M01D28.csv")
pop17<-fread("IHME_GBD_2019_POP_SYA_2017_Y2021M01D28.csv")
pop18<-fread("IHME_GBD_2019_POP_SYA_2018_Y2021M01D28.csv")
pop19<-fread("IHME_GBD_2019_POP_SYA_2019_Y2021M01D28.csv")

gbdpop<-rbindlist(list(pop0,pop1,pop2,pop3,pop4,pop5,pop6,pop7,
                       pop8,pop9,pop10,pop11,pop12,pop13,pop14,
                       pop15,pop16,pop17,pop18,pop19))%>%
  filter(sex_name!="both", location_id!=533)


#Maybe Georgia the state is a problem?
#533

gbd1<-fread("gbd1.csv")
gbd2<-fread("gbd2.csv")
gbd3<-fread("gbd3.csv")
gbd4<-fread("gbd4.csv")
gbd5<-fread("gbd5.csv")
gbd6<-fread("gbd6.csv")
gbd7<-fread("gbd7.csv")
gbd8<-fread("gbd8.csv")
gbd9<-fread("gbd9.csv")
gbd10<-fread("gbd10.csv")

gbd<-rbindlist(list(gbd1, gbd2, gbd3, gbd4, gbd5, gbd6, 
               gbd7, gbd8, gbd9, gbd10))

countrynames<-fread("Country_groupings_extended.csv")%>%
  select(c("gbd2019", "location_gbd"))%>%
  rename(location = location_gbd)

gbd<-left_join(gbd%>%rename(gbd2019 = location), countrynames, by="gbd2019")
gbd<-right_join(gbd, groups, by="location")

gbd<-gbd%>%filter(measure=="Deaths", metric=="Number", location%in%countrylist)
any(is.na(gbd))
unique(gbd$location)

gbdpop$age<-as.numeric(gbdpop$age_group_name)
gbdpop<-na.omit(gbdpop)
any(is.na(gbdpop))
gbdpop<-gbdpop%>%filter(age>=30 & age<70)
gbdpop$age.group[gbdpop$age>=30 & gbdpop$age<35]<-"30 to 34"
gbdpop$age.group[gbdpop$age>=35 & gbdpop$age<40]<-"35 to 39"
gbdpop$age.group[gbdpop$age>=40 & gbdpop$age<45]<-"40 to 44"
gbdpop$age.group[gbdpop$age>=45 & gbdpop$age<50]<-"45 to 49"
gbdpop$age.group[gbdpop$age>=50 & gbdpop$age<55]<-"50 to 54"
gbdpop$age.group[gbdpop$age>=55 & gbdpop$age<60]<-"55 to 59"
gbdpop$age.group[gbdpop$age>=60 & gbdpop$age<65]<-"60 to 64"
gbdpop$age.group[gbdpop$age>=65 & gbdpop$age<70]<-"65 to 69"

pop<-left_join(countrynames, gbdpop%>%rename(gbd2019 = location_name))

pop<-pop%>%filter(location%in%countrylist)
pop<-left_join(pop, groups, by="location")
any(is.na(pop))
unique(pop$location)

gbdpop<-pop%>%group_by(age.group, super_region, location, year_id)%>%
  summarize(pop=sum(val))%>%
  rename(age = age.group, year  = year_id)%>%
  filter(location %in% countrylist)

#50q30 by region
gbdWB<-gbd%>%filter(cause!="All causes")%>%
  group_by(age, super_region, year)%>%
  summarise(dead=sum(val))

gbdWB<-left_join(gbdpop%>%group_by(age, super_region, year)%>%
                   summarise(pop=sum(pop)), 
                 gbdWB, 
                 by=c("super_region", "year", "age"))

gbdWB$mx<-gbdWB$dead/gbdWB$pop

gbdWB<-gbdWB%>%group_by(super_region, year)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

gbdWB$location<-gbdWB$super_region

#50q30 by country
gbd_country<-gbd%>%filter(cause!="All causes")%>%
  group_by(age, location, super_region, year)%>%
  summarise(dead=sum(val))

gbd_country<-left_join(gbdpop%>%group_by(age, location, super_region, year)%>%summarise(pop=sum(pop)), 
                 gbd_country, 
                 by=c("location", "super_region", "year", "age"))

gbd_country$mx<-gbd_country$dead/gbd_country$pop
gbd_country<-gbd_country%>%group_by(location,super_region, year)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

unique(gbd$location)

#by continent
all_gbd<-gbd%>%filter(cause!="All causes")%>%
  group_by(age, year)%>%
  summarise(dead=sum(val))

all_gbd<-left_join(gbdpop%>%group_by(age, year)%>%summarise(pop=sum(pop)), 
                   all_gbd, 
                   by=c("year", "age"))

all_gbd$mx<-all_gbd$dead/all_gbd$pop

all_gbd<-all_gbd%>%group_by(year)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))%>%
  mutate(location = "Africa", super_region="Africa")


####################################################
#### add to modeled results and graph! ###
####################################################
plot<-bind_rows(WB_50q30%>%filter(year>2019)%>%mutate(location=super_region), 
                gbdWB,
                country_50q30%>%filter(year>2019), gbd_country,
                all_50q30%>%filter(year>2019), all_gbd)%>%
  select(-intervention)

any(is.na(plot))

##formatted graph##
library(ggthemes)

plot<-plot%>%
  mutate(label=ifelse(location %in% c("Africa", "Northern Africa", "Eastern Africa",
                                      "Western Africa", "Southern Africa", "Central Africa"), 
                      location, "other"),
         text = ifelse(year==2030 & label!="other", label, NA))%>%
  filter(year>=2015, year<=2030)

plot$label<-factor(plot$label, levels=c("Africa", "Central Africa", "Eastern Africa", "Northern Africa",
                                        "Southern Africa", "Western Africa", "other"))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")

ggplot(plot%>%filter(location!="Africa"), aes(x=year, y=x50q30*100, color=label, size=label, group=location))+
  geom_line()+
  ylab("CVD-specific 40q30 (%)")+
  scale_color_manual(values=c(cbPalette[2], cbPalette[6],
                              cbPalette[8], cbPalette[7], cbPalette[4], "grey"), guide='none')+
  scale_size_manual(values=c(1.5,1.5,1.5,1.5,1.5, 0.5), guide='none')+
  geom_text(aes(label=text), hjust=-0.1, size=3)+
  theme_bw()+
  xlab("Year")+
  xlim(2015,2034)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
ggsave("regions_40q30_2.jpeg", height=5, width=8)  



plot2<-plot%>%select(location, super_region, year, x50q30)%>%spread(year, x50q30)%>%
  mutate(base = `2015`)%>%
  gather(year, x50q30, -location, -super_region, -base)%>%
  mutate(year = as.numeric(year),
         diff = x50q30/base)

plot2<-plot2%>%
  mutate(label=ifelse(location %in% c("Africa", "Northern Africa", "Eastern Africa",
                                      "Western Africa", "Southern Africa", "Central Africa"), 
                      location, "country"),
         text = ifelse(year==2030 & label!="country", label, NA))%>%
  filter(year>=2015, year<=2030)

plot2$label<-factor(plot2$label, levels=c("Africa", "Central Africa", "Eastern Africa", "Northern Africa",
                                        "Southern Africa", "Western Africa", "country"))



ggplot(plot2%>%filter(location!="Africa"), 
       aes(x=year, y=diff, color=label, size=label, group=location))+
  geom_line()+
  ylab("Change in CVD-specific 40q30 relative to 2015")+
  scale_color_manual(values=c(cbPalette[2], cbPalette[6],
                              cbPalette[8], cbPalette[7], 
                              cbPalette[4], "grey"))+
  scale_size_manual(values=c(1.5,1.5,1.5,1.5,1.5, 0.5))+
  theme_bw()+
  xlab("Year")+
  xlim(2015,2030)+
  ylim(0.6,1.25)+
  labs(color="", size="")+
  geom_line(y=1, color="black", lty=2, size=0.7)+
  geom_line(y=0.667, color="black", lty=2, size=0.7)
  
ggsave("changein_40q30_2.jpeg", height=5, width=8)  

write.csv(plot2, "data_40q30.csv", row.names = F)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)

load("model/model_output_newnorm.Rda")

countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global", 
                                                                              location!="American Samoa",
                                                                              location!="Andorra",
                                                                              location!= "Bermuda",
                                                                              location!= "Dominica",
                                                                              location!="Greenland",
                                                                              location!="Marshall Islands",
                                                                              location!="Northern Mariana Islands",
                                                                              location!="Palestine",
                                                                              location!="Taiwan (Province of China)",
                                                                              location!="Guam",
                                                                              location!="Puerto Rico",
                                                                              location!="South Sudan",
                                                                              location!="Virgin Islands, U.S.")%>%pull(location)

comb<-bind_rows(progress%>%filter(intervention!="b.a.u")%>%
                  mutate(intervention = paste0("Progress_", intervention)),
                aspirational%>%filter(intervention!='b.a.u')%>%
                  mutate(intervention = paste0("Aspirational_", intervention)),
                progress%>%filter(intervention=='b.a.u')%>%
                  mutate(intervention = "Business as usual")
)

#WB income groups
groups<-read.csv("..model/Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%rename(location = location_gbd)

#########

comb<-left_join(comb, groups, by="location")
any(is.na(comb$wb2021))

CVD<-comb%>%group_by(age, sex, location, wb2021, year, intervention)%>%
  filter(age>=30 & age<80)%>%summarise(pop=sum(pop)/4, dead=sum(dead)) #divide pop by 4 to avoid over counting for each cause
CVD$age.group<-NA
CVD$age.group[CVD$age>=30 & CVD$age<35]<-"30-34"
CVD$age.group[CVD$age>=35 & CVD$age<40]<-"35-39"
CVD$age.group[CVD$age>=40 & CVD$age<45]<-"40-44"
CVD$age.group[CVD$age>=45 & CVD$age<50]<-"45-49"
CVD$age.group[CVD$age>=50 & CVD$age<55]<-"50-54"
CVD$age.group[CVD$age>=55 & CVD$age<60]<-"55-59"
CVD$age.group[CVD$age>=60 & CVD$age<65]<-"60-64"
CVD$age.group[CVD$age>=65 & CVD$age<70]<-"65-69"
CVD$age.group[CVD$age>=70 & CVD$age<75]<-"70-74"
CVD$age.group[CVD$age>=75 & CVD$age<80]<-"75-79"

#by region
WB_50q30<-CVD%>%group_by(age.group,  wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

ggplot(WB_50q30, aes(x=year, y=x50q30, color=intervention))+
  geom_point()+
  facet_wrap(~wb2021)

write.csv(WB_50q30, "../figures/x50q30_region_2022.csv")

#by country
country_50q30<-CVD%>%group_by(age.group,location, wb2021, year, intervention)%>%summarise(pop=sum(pop), dead=sum(dead))
country_50q30$mx<-country_50q30$dead/country_50q30$pop
any(is.na(country_50q30))

country_50q30<-country_50q30%>%group_by(location,wb2021, year, intervention)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

ggplot(country_50q30%>%filter(wb2021=="LIC", intervention=="Business as usual"), 
       aes(x=year, y=x50q30, color=location))+
  geom_point()

write.csv(country_50q30, "../figures/x50q30_country_2022.csv")


##all countries
all_50q30<-CVD%>%group_by(age.group, year, intervention)%>%summarise(pop=sum(pop), dead=sum(dead))
all_50q30$mx<-all_50q30$dead/all_50q30$pop
any(is.na(all_50q30))

all_50q30<-all_50q30%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

ggplot(all_50q30, 
       aes(x=year, y=x50q30, color=intervention))+
  geom_point()

write.csv(all_50q30, "../figures/all_50q30_2022.csv")

xx50q30<-bind_rows(all_50q30%>%mutate(location="World"), country_50q30,
                   WB_50q30)%>%mutate(sex="Both")
#female
WB_50q30_f<-CVD%>%filter(sex=="Female")%>%
  group_by(age.group,  wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30_f$mx<-WB_50q30_f$dead/WB_50q30_f$pop
any(is.na(WB_50q30_f))

WB_50q30_f<-WB_50q30_f%>%group_by(wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
#
country_50q30_f<-CVD%>%filter(sex=="Female")%>%
  group_by(age.group,location, wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
country_50q30_f$mx<-country_50q30_f$dead/country_50q30_f$pop
any(is.na(country_50q30_f))

country_50q30_f<-country_50q30_f%>%group_by(location,wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
#
all_50q30_f<-CVD%>%filter(sex=="Female")%>%
  group_by(age.group, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
all_50q30_f$mx<-all_50q30_f$dead/all_50q30_f$pop
any(is.na(all_50q30_f))

all_50q30_f<-all_50q30_f%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

fx50q30<-bind_rows(all_50q30_f%>%mutate(location="World"), country_50q30_f,
                   WB_50q30_f)%>%mutate(sex="Female")

#male
WB_50q30_m<-CVD%>%filter(sex=="Male")%>%
  group_by(age.group,  wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30_m$mx<-WB_50q30_m$dead/WB_50q30_m$pop
any(is.na(WB_50q30_m))

WB_50q30_m<-WB_50q30_m%>%group_by(wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
#
country_50q30_m<-CVD%>%filter(sex=="Male")%>%
  group_by(age.group,location, wb2021, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
country_50q30_m$mx<-country_50q30_m$dead/country_50q30_m$pop
any(is.na(country_50q30_m))

country_50q30_m<-country_50q30_m%>%group_by(location,wb2021, year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
#
all_50q30_m<-CVD%>%filter(sex=="Male")%>%
  group_by(age.group, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
all_50q30_m$mx<-all_50q30_m$dead/all_50q30_m$pop
any(is.na(all_50q30_m))

all_50q30_m<-all_50q30_m%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

mx50q30<-bind_rows(all_50q30_m%>%mutate(location="World"), country_50q30_m,
                   WB_50q30_m)%>%mutate(sex="Male")

xx50q30<-bind_rows(xx50q30, fx50q30, mx50q30)

write.csv(xx50q30, "../web_appendix/shiny/x_50q30_2022.csv")


##add GBD historic data ###
setwd("~/RTSL/data_preprocessing/2. get AARCs")
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
  select(c("Super_region", "gbd2019", "location_gbd"))%>%
  rename(location = location_gbd,
         super_region = Super_region)

gbd<-left_join(gbd%>%rename(gbd2019 = location), countrynames, by="gbd2019")
gbd<-left_join(gbd, groups, by="location")

gbd<-gbd%>%filter(measure=="Deaths", metric=="Number", location%in%countrylist)
any(is.na(gbd))
unique(gbd$location)

gbdpop$age<-as.numeric(gbdpop$age_group_name)
gbdpop<-na.omit(gbdpop)
any(is.na(gbdpop))
gbdpop<-gbdpop%>%filter(age>=30 & age<80)
gbdpop$age.group[gbdpop$age>=30 & gbdpop$age<35]<-"30 to 34"
gbdpop$age.group[gbdpop$age>=35 & gbdpop$age<40]<-"35 to 39"
gbdpop$age.group[gbdpop$age>=40 & gbdpop$age<45]<-"40 to 44"
gbdpop$age.group[gbdpop$age>=45 & gbdpop$age<50]<-"45 to 49"
gbdpop$age.group[gbdpop$age>=50 & gbdpop$age<55]<-"50 to 54"
gbdpop$age.group[gbdpop$age>=55 & gbdpop$age<60]<-"55 to 59"
gbdpop$age.group[gbdpop$age>=60 & gbdpop$age<65]<-"60 to 64"
gbdpop$age.group[gbdpop$age>=65 & gbdpop$age<70]<-"65 to 69"
gbdpop$age.group[gbdpop$age>=70 & gbdpop$age<75]<-"70 to 74"
gbdpop$age.group[gbdpop$age>=75 & gbdpop$age<80]<-"75 to 79"

pop<-left_join(countrynames, gbdpop%>%rename(gbd2019 = location_name))

pop<-pop%>%filter(location%in%countrylist)
pop<-left_join(pop, groups, by="location")
any(is.na(pop))
unique(pop$location)

gbdpop<-pop%>%group_by(age.group, location, wb2021, year_id)%>%summarize(pop=sum(val))%>%
  rename(age = age.group, year  = year_id)

#50q30 by region
gbdWB<-gbd%>%filter(cause!="All causes")%>%group_by(age, wb2021, year)%>%summarise(dead=sum(val))
gbdWB<-left_join(gbdpop%>%group_by(age, wb2021, year)%>%summarise(pop=sum(pop)), 
                 gbdWB, 
                 by=c("wb2021", "year", "age"))
gbdWB$mx<-gbdWB$dead/gbdWB$pop
gbdWB<-gbdWB%>%group_by(wb2021, year)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
gbdWB$intervention<-"Historical"

#50q30 by country
gbd_country<-gbd%>%filter(cause!="All causes")%>%group_by(age, location, wb2021, year)%>%summarise(dead=sum(val))
gbd_country<-left_join(gbdpop%>%group_by(age, location, wb2021, year)%>%summarise(pop=sum(pop)), 
                 gbd_country, 
                 by=c("location", "wb2021", "year", "age"))
gbd_country$mx<-gbd_country$dead/gbd_country$pop
gbd_country<-gbd_country%>%group_by(location,wb2021, year)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
gbd_country$intervention<-"Historical"

##all countries
gbd_all<-gbd%>%filter(cause!="All causes")%>%group_by(age, year)%>%summarise(dead=sum(val))
gbd_all<-left_join(gbdpop%>%group_by(age, year)%>%summarise(pop=sum(pop)), 
                   gbd_all,by=c("year", "age"))
gbd_all$mx<-gbd_all$dead/gbd_all$pop
gbd_all<-gbd_all%>%group_by(year)%>%summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))
gbd_all$intervention<-"Historical"

all_50q30<-bind_rows(all_50q30, gbd_all)

ggplot(all_50q30, 
       aes(x=year, y=x50q30, color=intervention))+
  geom_point()

write.csv(all_50q30, "../../figures/all_50q30_2022.csv")

p<-all_50q30%>%filter(year==2020)%>%pull(x50q30, intervention)
p2<-all_50q30%>%filter(year==2023)%>%pull(x50q30, intervention)
p3<-all_50q30%>%filter(year==2050)%>%pull(x50q30, intervention)

p2[2]
p3[2]
#Average annual rate of change in each of the three scenarios
#BAU
100*log(p3[4]/p2[4])/27
#100*log(0.1745869/0.1933050)/27

#Progress
100*log(p3[6]/p2[6])/27
#100*log(0.1610327 /0.1933050)/27

#Aspirational
100*log(p3[2]/p2[2])/27
#100*log( 0.1498949/0.1933050)/27

(p3[6]-p3[4])/p3[4]
#(0.1610327-0.1745869)/0.1745869
(p3[2]-p3[4])/p3[4]
#(0.1498949 -0.1745869)/0.1745869

####################################################
#### add to modeled results and graph! ###
####################################################

plot_WB<-bind_rows(WB_50q30%>%filter(year>=2019), gbdWB)
plot_WB2<-bind_rows(WB_50q30%>%filter(year>=2022), gbdWB)
plot_country<-bind_rows(country_50q30%>%filter(year>=2022), gbd_country)#%>%filter(year<=2017))

##
plot_country<-plot_country%>%mutate(intervention = ifelse(intervention=="Progress_Both", "Progress",
                                                          ifelse(intervention=="Aspirational_Both", "Aspirational", intervention)))
plot_country$intervention<-factor(plot_country$intervention, 
                             levels=c("Historical", "Business as usual", 
                                      "Progress_Salt reduction", "Aspirational_Salt reduction","Progress", 
                                      "Progress_Antihypertensive therapy", "Aspirational_Antihypertensive therapy", 
                                      "Aspirational")) 
plot_country<-plot_country%>%rename(Scenario = intervention)

plot_WB<-plot_WB%>%mutate(intervention = ifelse(intervention=="Progress_Both", "Progress",
                                             ifelse(intervention=="Aspirational_Both", "Aspirational", intervention)))
plot_WB$intervention<-factor(plot_WB$intervention, 
                             levels=c("Historical", "Business as usual", 
                                      "Progress_Salt reduction", "Aspirational_Salt reduction","Progress", 
                                      "Progress_Antihypertensive therapy", "Aspirational_Antihypertensive therapy", 
                                      "Aspirational")) 
plot_WB$wb2021<-factor(plot_WB$wb2021,
                          levels=c("LIC", "LMIC", "UMIC", "HIC"))
plot_WB<-plot_WB%>%rename(Scenario = intervention)


plot_WB2<-plot_WB2%>%mutate(intervention = ifelse(intervention=="Progress_Both", "Progress",
                                                ifelse(intervention=="Aspirational_Both", "Aspirational", intervention)))
plot_WB2$intervention<-factor(plot_WB2$intervention, 
                              levels=c("Historical", "Business as usual", 
                                       "Progress_Salt reduction", "Aspirational_Salt reduction","Progress", 
                                       "Progress_Antihypertensive therapy", "Aspirational_Antihypertensive therapy", 
                                       "Aspirational")) 
plot_WB2$wb2021<-factor(plot_WB2$wb2021,
                       levels=c("LIC", "LMIC", "UMIC", "HIC"))
plot_WB2<-plot_WB2%>%rename(Scenario = intervention)


##formatted graph##
library(ggthemes)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")
#from: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette


ggplot(plot_country%>%filter(year>=2010 & location=="China",
                             Scenario%in%c("Historical", "Business as usual", "Progress", "Aspirational")), 
       aes(x=year, y=x50q30, color=Scenario))+
  #facet_wrap(~wb2021, nrow=1, labeller = labeller(wb2021 = label_wrap_gen(10)))+
  #geom_point()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(7,4,2,3)])+
  xlab("Year")+
  ylab("Probability of dying between ages 30 and 80")+
  ggtitle("CVD-specific 50q30")+
  ylim(0,0.35)

write.csv(plot_country, "../../figures/country_50q30_2020.csv")

#ggsave("../../figures/Figure4_alt_2021.png", width=8, height=6)


ggplot(plot_WB2%>%filter(year>=2010,
                         Scenario%in%c("Historical", "Business as usual", "Progress", "Aspirational")), 
       aes(x=year, y=x50q30, color=Scenario))+
  facet_wrap(~wb2021, nrow=1, labeller = labeller(wb2021 = label_wrap_gen(10)))+
  #geom_point()+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  theme_calc()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  scale_colour_manual(values=cbPalette[c(7,4,2,3,6)])+
  xlab("Years (2010 to 2050)")+
  ylab("Probability of dying between ages 30 and 80")+
  ggtitle("CVD-specific 50q30 from 2010 to 2050 by World Bank Income Group")+
  ylim(0,0.35)

ggsave("../../figures/Figure4_ideal2020.png", width=8, height=6)

ggplot(plot_WB2%>%filter(year>=2010 & 
                           Scenario%in%c("Historical", "Business as usual", "Progress", "Aspirational")), 
       aes(x=year, y=x50q30, color=Scenario))+
  facet_wrap(~wb2021, nrow=1, labeller = labeller(wb2021 = label_wrap_gen(10)))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45))+
  scale_colour_manual(values=cbPalette[c(7,4,2,3,6)])+
  xlab("Years (2010 to 2050)")+
  ylab("Probability of dying from CVD between ages 30 and 80 years")+
  ylim(0,0.35)

ggsave("../../figures/Figure4_2022.png", width=8, height=5)


ggplot(plot_WB%>%filter(year>=2010 & 
                           Scenario%in%c("Historical", "Business as usual", "Progress", "Aspirational")), 
       aes(x=year, y=x50q30, color=Scenario))+
  facet_wrap(~wb2021, nrow=1, labeller = labeller(wb2021 = label_wrap_gen(10)))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45))+
  scale_colour_manual(values=cbPalette[c(7,4,2,3,6)])+
  xlab("Years (2010 to 2050)")+
  ylab("Probability of dying from CVD between ages 30 and 80 years")+
  ylim(0,0.35)

ggsave("../../figures/Figure4_connected_2022.png", width=8, height=5)


write.csv(plot_WB, "../../figures/resutls_50q30_2020.csv", row.names = F)
#plot_WB<-read.csv("figures/resutls_50q30.csv", stringsAsFactors = F)

lic20<-plot_WB%>%filter(year==2020, wb2021=="LIC", Scenario =="Business as usual")%>%pull(x50q30)
licbau<-plot_WB%>%filter(year==2050, wb2021=="LIC", Scenario =="Business as usual")%>%pull(x50q30)
licref<-plot_WB%>%filter(year==2050, wb2021=="LIC", Scenario =="Progress")%>%pull(x50q30)
licasp<-plot_WB%>%filter(year==2050, wb2021=="LIC", Scenario =="Aspirational")%>%pull(x50q30)

#change in 50q30
100*(licref-lic20)/30
100*(licasp-lic20)/30
100*(licbau-lic20)/30

hic20<-plot_WB%>%filter(year==2020, wb2021=="HIC", Scenario =="Business as usual")%>%pull(x50q30)
hicbau<-plot_WB%>%filter(year==2050, wb2021=="HIC", Scenario =="Business as usual")%>%pull(x50q30)
hicref<-plot_WB%>%filter(year==2050, wb2021=="HIC", Scenario =="Progress")%>%pull(x50q30)
hicasp<-plot_WB%>%filter(year==2050, wb2021=="HIC", Scenario =="Aspirational")%>%pull(x50q30)

100*(hicref-hic20)/30
100*(hicasp-hic20)/30
100*(hicbau-hic20)/30

lic20
hic20

lic20-hic20
licbau-hicbau
licref-hicref
licasp-hicasp

unique(plot_country$location[plot_country$wb2021=="LIC"])
###Why LIC so off?###
ggplot(plot_country%>%filter(year>=2010, 
                             Scenario%in%c("Business as usual","Historical"),
                             wb2021=="LIC"),
       aes(x=year, y=x50q30, color=location))+
  geom_point()+
  theme_calc()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("Years (2010 to 2050)")+
  ylab("Probability of dying between ages 30 and 80")+
  ggtitle("CVD-specific 50q30 from 2010 to 2050 by country (LIC)")

#ggsave("../Figure4_LIC.png", width=8, height=6)


#######################################
##table of slopes##
#######################################
tab4<-plot_WB2%>%filter((year==2022 | year==2050))%>%
  spread(year, x50q30)%>%
  group_by(wb2021, Scenario)%>%
  summarise(change = 100*(`2050`-`2022`)/28)%>%
  spread(Scenario, change)

basetab<-na.omit(plot_WB2%>%filter(Scenario=="Historical" &
                         (year==2000 | year==2019)))

basetab<-basetab%>%spread(year, x50q30)%>%
  group_by(wb2021, Scenario)%>%
  summarise(change = 100*(`2019`-`2000`)/19)%>%
  spread(Scenario, change)%>%
  rename(historic = `Historical`)


tab4<-left_join(tab4, basetab)
write.csv(tab4, "../../figures/x50q30_slopes_2022.csv")

#######################################
###TABLE 2###
#######################################
x50q30<-country_50q30
########################################
tab22<-data.frame(
  Scenario = c("BAU2020", "Business as usual 2050", "Progress 2050", "Aspirational 2050", "Ideal 2050"),
  X10 = c(as.numeric(quantile(x50q30%>%filter(year==2020 & intervention=="Business as usual")%>%pull(x50q30), 0.1, na.rm = T)),
          as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Business as usual")%>%pull(x50q30),0.1, na.rm = T)),
          as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Progress_Both")%>%pull(x50q30),0.1, na.rm = T)),
          as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Aspirational_Both")%>%pull(x50q30),0.1, na.rm = T)),
          as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Ideal")%>%pull(x50q30), 0.1, na.rm = T))
  ),
  X90= c(as.numeric(quantile(x50q30%>%filter(year==2020 & intervention=="Business as usual")%>%pull(x50q30),0.9, na.rm = T)),
         as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Business as usual")%>%pull(x50q30),0.9, na.rm = T)),
         as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Progress_Both")%>%pull(x50q30),0.9, na.rm = T)),
         as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Aspirational_Both")%>%pull(x50q30),0.9, na.rm = T)),
         as.numeric(quantile(x50q30%>%filter(year==2050 & intervention=="Ideal")%>%pull(x50q30),0.9, na.rm = T))
  ),
  SD = c(sd(x50q30%>%filter(year==2020 & intervention=="Business as usual")%>%pull(x50q30)),
         sd(x50q30%>%filter(year==2050 & intervention=="Business as usual")%>%pull(x50q30)),
         sd(x50q30%>%filter(year==2050 & intervention=="Progress_Both")%>%pull(x50q30)),
         sd(x50q30%>%filter(year==2050 & intervention=="Aspirational_Both")%>%pull(x50q30)),
         sd(x50q30%>%filter(year==2050 & intervention=="Ideal")%>%pull(x50q30))
  )
)%>%mutate(Gap = X90-X10)

write.csv(tab22, "../../figures/Table2_90_2022.csv")


#by WB group
tab2<-plot_WB%>%filter(year==2050 | 
                      (year==2020 & 
                       Scenario=="Business as usual")
                      )%>%
                filter(wb2021=="HIC" | wb2021=="LIC")%>%
                spread(wb2021,x50q30)%>%
  mutate(Gap = LIC-HIC)%>%
  filter(Scenario%in%c("Business as usual", "Progress", "Aspirational"))

#add sd
tab2<-cbind(tab2, tab22[1:4,"SD"])
names(tab2)[6]<-"Standard deviation"

write.csv(tab2, "../../figures/Table2_2022.csv")


#######################################
## REDID THIS IN newfigs2.R
#DELETE ?
########## newest plot (Figure2) ##########
#######################################

#assumed 2* high BP in CVD than not
setwd("C:/Users/sarah/Documents/RTSL/data_preprocessing")
data<-bind_rows(int1, int2, int3)#, int4)
#bind with BP data
data.in<-fread("bp_data3.csv")
data.in$salt[data.in$location=="China"]<-4.83*2.54

names<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, iso3)%>%
  rename(location = location_gbd)

data.in<-left_join(data.in, names, by = "location" )

setwd("C:/Users/sarah/Documents/RTSL/model")
#add increase data
inc<-read.csv("coverage_increases.csv", stringsAsFactors = F)%>%
  rename(iso3 = ISO)

data.in<-left_join(data.in, inc, by="iso3")
data.in<-data.in%>%arrange(location,age,sex)%>%
  filter(location%in%countrylist)

source("functions_review_4.R")

repYear<-function(row){
  2017+floor((row-1)/224)#40768)
}
####
#call fxn
###

bps<-data.frame(
  intervention = character(),
  age = character(),
  sex = character(),
  location = character(),
  Year = numeric(),
  bp_cat = character(),
  prob = numeric()
)

for (i in countrylist){
DT<-unique(data.in[location==i][,Year:=2017][,-c("Low95CI", "High95CI", "V1")])

DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
bp_prob_ref<-na.omit(get.bp.prob(DT.in, 0.3, "percent", 2023, 2025, 1, "p75"))
bp_prob_ref$intervention<-"Progress"
DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
bp_prob_asp<-get.bp.prob(DT.in, 7.56, "target", 2023, 2025, 1, "p975")
bp_prob_asp$intervention<-"Aspirational"
DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
bp_prob_bau<-get.bp.prob(DT.in, 0, 'percent', 2023, saltyear2, 1, "baseline")
bp_prob_bau$intervention<-"Business as usual"
DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]

bps<-bind_rows(bps, bp_prob_ref, bp_prob_asp, bp_prob_bau)

}

normo<-bps%>%filter(bp_cat%in%c("<120", "120-129", "130-139"))%>%
  group_by(location, Year, age, sex, intervention)%>%
  summarise(prop.norm = sum(prob))

'%!in%' <- function(x,y)!('%in%'(x,y))

hyper<-bps%>%filter(bp_cat%!in%c("<120", "120-129", "130-139"))%>%
  group_by(location, Year, age, sex, intervention)%>%
  summarise(prop.hyp = sum(prob))
  
htn<-left_join(normo, hyper)%>%
  mutate(check = prop.norm + prop.hyp)
sum(htn$check)
unique(htn$age)
####
#add to deaths/prevalence 
###
data$age.group[data$age>=20 & data$age<25]<-"20-24"
data$age.group[data$age>=25 & data$age<30]<-"25-29"
data$age.group[data$age>=30 & data$age<35]<-"30-34"
data$age.group[data$age>=35 & data$age<40]<-"35-39"
data$age.group[data$age>=40 & data$age<45]<-"40-44"
data$age.group[data$age>=45 & data$age<50]<-"45-49"
data$age.group[data$age>=50 & data$age<55]<-"50-54"
data$age.group[data$age>=55 & data$age<60]<-"55-59"
data$age.group[data$age>=60 & data$age<65]<-"60-64"
data$age.group[data$age>=65 & data$age<70]<-"65-69"
data$age.group[data$age>=70 & data$age<75]<-"70-74"
data$age.group[data$age>=75 & data$age<80]<-"75-79"
data$age.group[data$age>=80 & data$age<85]<-"80-84"
data$age.group[data$age>=85]<-"85plus"

data<-data%>%group_by(year, intervention, age.group, sex, location)%>%
  summarise(pop=sum(pop)/4, all.mx=sum(all.mx)/4, well=sum(well), 
            dead=sum(dead), newcases=sum(newcases),
            sick=sum(sick))%>%
  rename(age = age.group)

df<-left_join(data, htn%>%rename(year = Year))

df$well[df$well==0]<-1

#assume 2times as likely to have high bp if sick(w/CVD)
df<-df%>%mutate(HTN = (pop-all.mx)*prop.hyp,
                htn_nocvd = HTN / ((2*sick/well) +1),
                htn_cvd = HTN - htn_nocvd,
                check = HTN - htn_nocvd - htn_cvd,
                check2 = ifelse(sick<htn_cvd,1,0),
                check3 = ifelse(well<htn_nocvd,1,0)
                )

plot<-df%>%
  group_by(year, age, intervention)%>%
  summarise(dead = sum(all.mx),
            cvd = sum(sick),
            htn = sum(htn_nocvd),
            pop = sum(pop))


plot<-plot%>%filter((age%in%c("35-39") & year==2020) |
                    (age%in%c("40-44") & year==2025) |
                    (age%in%c("45-49") & year==2030) |
                    (age%in%c("50-54") & year==2035) |
                    (age%in%c("55-59") & year==2040) |
                    (age%in%c("60-64") & year==2045) |
                    (age%in%c("65-69") & year==2050)
                    
                    )%>%
  group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            cvd=sum(cvd),
            htn=sum(htn),
            pop = sum(pop),
            well = pop - htn - cvd)

#make dead absorbing state
plot$pop20[plot$intervention=="Business as usual"]<-
  plot$pop[plot$intervention=="Business as usual" & plot$year==2020]
plot$pop20[plot$intervention=="Progress"]<-
  plot$pop[plot$intervention=="Progress" & plot$year==2020]
plot$pop20[plot$intervention=="Aspirational"]<-
  plot$pop[plot$intervention=="Aspirational" & plot$year==2020]

plot<-plot%>%mutate(dead = pop20 - well - cvd - htn,
                    well = well/pop20,
                    cvd = cvd/pop20,
                    htn = htn/pop20,
                    dead = dead/pop20)%>%
  select(-c(pop, pop20))%>%
  gather(State, proportion, -year, -intervention)

plot$Scenario <- factor(plot$intervention,
                        levels = c("Business as usual",
                                   "Progress",
                                   "Aspirational"))

plot$State[plot$State == "cvd"]<-"Prevalent CVD"
plot$State[plot$State == "dead"]<-"All cause death"
plot$State[plot$State == "htn"]<-"Raised blood pressure without CVD"
plot$State[plot$State == "well"]<-"Alive without hypertension or CVD"

plot$State <- factor(plot$State,
                        levels = c("Alive without hypertension or CVD",
                                   "Raised blood pressure without CVD",
                                   "Prevalent CVD",
                                   "All cause death"))

ggplot(plot, aes(x=year, y=proportion, 
                 color=State, linetype=Scenario))+
  geom_line(size = 0.7)+
  ylab("Proportion")+
  xlab("Year")+
  theme_bw()+
  scale_linetype_manual(values=c("solid", "longdash", 
                                 "dotted", "dotdash"))


ggsave("../figures/Figure2_new.png", height = 6, width=8)


### #s
plot$proportion[plot$State=="Alive without hypertension or CVD" & plot$year==2050]

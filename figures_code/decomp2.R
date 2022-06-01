setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readxl)

load("../model/model_output_newnorm.Rda")

df<-bind_rows(progress%>%filter(intervention!="b.a.u")%>%
                  mutate(intervention = paste0("Progress_", intervention)),
                aspirational%>%filter(intervention!='b.a.u')%>%
                  mutate(intervention = paste0("Aspirational_", intervention)),
                progress%>%filter(intervention=='b.a.u')%>%
                  mutate(intervention = "Business as usual")
)
#int4$intervention<-"Ideal"
#########demographic decompostion ############
region<-read.csv("super_regions.csv", stringsAsFactors = F)
df<-left_join(df, region, by="location")

#WB income group
#WB income groups
groups<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%rename(location = location_gbd)

df<-left_join(df, groups, by="location")
any(is.na(df$wb2021))

#adding all countries and all cvd causes categories#
#causedeath is number of deaths from a specific cause vs. all.mx is all cause mortality
allc<-df%>%group_by(cause, age, year, intervention)%>%
  summarise(pop=sum(pop), all.mx=sum(all.mx), causedeath=sum(newcases))
allc$wb2021<-"All countries"
supereg<-df%>%group_by(cause, age, year, intervention, wb2021)%>%
  summarise(pop=sum(pop), all.mx=sum(all.mx), causedeath=sum(newcases))
df<-bind_rows(allc, supereg)

allcvd<-df%>%group_by(age, year, intervention, wb2021)%>%
  summarise(causedeath=sum(causedeath))
allcvd$cause<-"all cvd"
alldeaths<-df%>%ungroup%>%filter(cause=="ihd")%>%dplyr::select(age, year, intervention, wb2021, all.mx)
alldeaths$cause<-"all"
names(alldeaths)
names(alldeaths)[5]<-"causedeath"

#pull data for year 2020
deaths2020<-bind_rows(df%>%dplyr::select(age,year,causedeath,intervention, wb2021,cause),allcvd,alldeaths)%>%
  filter(year==2020)%>%
  group_by(cause,intervention, wb2021)%>%
  summarize("deaths2020"=sum(causedeath))

#pull data for year 2050
deaths2050<-bind_rows(df%>%dplyr::select(age,year,causedeath, intervention, wb2021,cause),allcvd,alldeaths)%>%
  filter(year==2050)%>%
  group_by(cause, intervention, wb2021)%>%
  summarize(deaths2050=sum(causedeath))

output<-left_join(deaths2020, deaths2050, by=c("cause", "intervention", "wb2021"))

#calculate the change from 2020 to 2050 from model estimates
output$change<-((output$deaths2050-output$deaths2020)/output$deaths2020)*100

##Scenario 1: use 2050 total pop w/ 2020 pop age and death rates
pop2050<-df%>%filter(cause=="ihd", year==2050)%>%group_by(intervention, wb2021)%>%summarise(totalpop2050=sum(pop))

deaths<-bind_rows(df%>%dplyr::select(age,year,causedeath,cause, intervention, wb2021),allcvd,alldeaths)%>%filter(year==2020)

growth<-df%>%filter(year==2020, cause=="ihd")
growth<-left_join(deaths, growth%>%dplyr::select(age, pop, intervention, wb2021), by=c("age", "intervention", "wb2021", "year"))
growth<-growth%>%ungroup%>%dplyr::select(-c(cause.y, year))%>%rename(cause = cause.x)

growth$deathrt<-growth$causedeath/growth$pop

pop2020<-growth%>%filter(cause=="ihd")%>%group_by(intervention, wb2021)%>%summarise(totalpop2020=sum(pop))

growth<-left_join(growth, pop2020, by=c("intervention", "wb2021"))
growth$ageprop<-growth$pop/growth$totalpop2020
sum(growth$ageprop[growth$cause=="ihd"])

growth<-left_join(growth, pop2050, by=c("intervention", "wb2021"))
growth$newpop<-growth$ageprop*growth$totalpop2050

growth$deathsexpgrowth<-growth$newpop*growth$deathrt
growth<-growth%>%group_by(cause, intervention, wb2021)%>%summarise(growthdeaths=sum(deathsexpgrowth))

output<-left_join(output, growth, by=c("cause", "intervention", "wb2021"))
output$change_growth<-((output$growthdeaths-output$deaths2020)/output$deaths2020)*100

##Scenario 2: use 2050 pop and age structure with 2020 death rates
aging<-df%>%filter(year==2020, cause=="ihd")
aging<-left_join(deaths, aging%>%dplyr::select(age, pop, intervention, wb2021), by=c("age", "intervention", "wb2021", "year"))
aging<-aging%>%dplyr::select(-c(cause.y, year))%>%rename(cause = cause.x)

aging$deathrt<-aging$causedeath/aging$pop

aging<-left_join(aging%>%dplyr::select(-c(pop, year)), 
                 df%>%filter(cause=="ihd", year==2050)%>%
                   group_by(age, intervention, wb2021)%>%
                   summarise(pop=sum(pop)), 
                 by=c("age", "intervention", "wb2021")) 

#aging<-aging%>%select(-c(cause.y, year))%>%rename(cause = cause.x)

aging$agedeaths<-aging$pop*aging$deathrt
aging<-aging%>%group_by(cause, intervention, wb2021)%>%summarise(agedeaths=sum(agedeaths))

output<-left_join(output, aging, by=c("cause", "intervention", "wb2021"))
output$change_age<-(((output$agedeaths-output$deaths2020)/output$deaths2020)*100)-output$change_growth

output$change_epi<-output$change-output$change_age-output$change_growth

names(output)[6]<-"Percent change in new cases (2020-2050)"
names(output)[7]<-"New cases expected using 2050 total pop, 2020 pop age structure, 2020 age- and sex-specific incidence rates"
names(output)[8]<-"Percent change from 2020 due to growth"
names(output)[9]<-"Deaths expected using 2050 total pop, 2050 pop age structure, 2020 age- and sex-specific incidence rates"
names(output)[10]<-"Percent change from 2020 due to aging"
names(output)[11]<-"Percent change from 2020 due to age-specific incidence rates"


#setwd("~/RTSL/web_appendix")
#write.csv(output, "decomp_data_BG.csv", row.names = F)

### graph time! ###

plot<-output[,c(1,2,3,6,8,10,11)]
names(plot)[4]<-"Net change in new cases"
names(plot)[5]<-"Change due to population growth"
names(plot)[6]<-"Change due to population aging"
names(plot)[7]<-"Change due to age-specific cvd incidence rates"

plot<-plot%>%mutate(intervention = ifelse(intervention=="Progress_Both", "Progress",
                                          ifelse(intervention=="Aspirational_Both", "Aspirational", intervention)))
plot$intervention2<-factor(plot$intervention,levels = c("Business as usual", "Progress", "Progress_Salt reduction",
                                                        "Progress_Antihypertensive therapy", "Aspirational_Salt reduction",
                                                        "Aspirational_Antihypertensive therapy","Aspirational"))

plot<-plot%>%filter(cause=="all cvd")%>%
  gather(metric, change, -wb2021, -intervention, -cause, -`Net change in new cases`, -intervention2)

plot$metric<-factor(plot$metric,levels = c("Change due to population aging", 
                                           "Change due to population growth", 
                                           "Change due to age-specific cvd incidence rates"))


plot$wb2021<-factor(plot$wb2021, levels = c("All countries",
                                            "LIC",
                                            "LMIC",
                                            "UMIC", "HIC"))
library(ggthemes)

p<-ggplot(plot%>%filter(intervention2%in%c("Business as usual", "Progress", "Aspirational")), 
          aes(x=intervention2))+
  geom_bar(stat='identity', aes(y=change, fill=metric), position="stack")+
  geom_point(aes(y=`Net change in new cases`,shape = "Net change"))+
  facet_grid(~wb2021, labeller = labeller(wb2021= label_wrap_gen(width=16)))+
  theme_calc()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom", legend.box = "horizontal", legend.direction = "horizontal")+
  scale_fill_manual(values = c("#56B4E9","#E69F00","#009E73"))+
  #theme_calc()+
  xlab("")+
  ylab("Change in incidence 2020 to 2050 (%)")+
  labs(fill="", shape=NULL)+
  ylim(c(-100,250))

p

ggsave("../output/fig_2.pdf", width=11, height=6, dpi=600)
write.csv(plot, "../output/decomp_data_2022.csv")


###deaths###
df<-bind_rows(progress%>%filter(intervention!="b.a.u")%>%
                mutate(intervention = paste0("Progress_", intervention)),
              aspirational%>%filter(intervention!='b.a.u')%>%
                mutate(intervention = paste0("Aspirational_", intervention)),
              progress%>%filter(intervention=='b.a.u')%>%
                mutate(intervention = "Business as usual")
)
df<-left_join(df, region, by="location")

#WB income group
#WB income groups
groups<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%rename(location = location_gbd)

df<-left_join(df, groups, by="location")
any(is.na(df$wb2021))

allc<-df%>%group_by(cause, age, year, intervention)%>%
  summarise(pop=sum(pop), all.mx=sum(all.mx), causedeath=sum(dead))
allc$wb2021<-"All countries"
supereg<-df%>%group_by(cause, age, year, intervention, wb2021)%>%
  summarise(pop=sum(pop), all.mx=sum(all.mx), causedeath=sum(dead))
df<-bind_rows(allc, supereg)

allcvd<-df%>%group_by(age, year, intervention, wb2021)%>%
  summarise(causedeath=sum(causedeath))
allcvd$cause<-"all cvd"
alldeaths<-df%>%ungroup%>%filter(cause=="ihd")%>%dplyr::select(age, year, intervention, wb2021, all.mx)
alldeaths$cause<-"all"
names(alldeaths)
names(alldeaths)[5]<-"causedeath"

#pull data for year 2020
deaths2020<-bind_rows(df%>%dplyr::select(age,year,causedeath,intervention, wb2021,cause),allcvd,alldeaths)%>%
  filter(year==2020)%>%
  group_by(cause,intervention, wb2021)%>%
  summarize("deaths2020"=sum(causedeath))

#pull data for year 2050
deaths2050<-bind_rows(df%>%dplyr::select(age,year,causedeath, intervention, wb2021,cause),allcvd,alldeaths)%>%
  filter(year==2050)%>%
  group_by(cause, intervention, wb2021)%>%
  summarize(deaths2050=sum(causedeath))

output<-left_join(deaths2020, deaths2050, by=c("cause", "intervention", "wb2021"))

#calculate the change from 2020 to 2050 from model estimates
output$change<-((output$deaths2050-output$deaths2020)/output$deaths2020)*100


##Scenario 1: use 2050 total pop w/ 2020 pop age and death rates
pop2050<-df%>%filter(cause=="ihd", year==2050)%>%group_by(intervention, wb2021)%>%summarise(totalpop2050=sum(pop))

deaths<-bind_rows(df%>%dplyr::select(age,year,causedeath,cause, intervention, wb2021),allcvd,alldeaths)%>%filter(year==2020)

growth<-df%>%filter(year==2020, cause=="ihd")
growth<-left_join(deaths, growth%>%dplyr::select(age, pop, intervention, wb2021), by=c("age", "intervention", "wb2021", "year"))
growth<-growth%>%ungroup%>%dplyr::select(-c(cause.y, year))%>%rename(cause = cause.x)

growth$deathrt<-growth$causedeath/growth$pop

pop2020<-growth%>%filter(cause=="ihd")%>%group_by(intervention, wb2021)%>%summarise(totalpop2020=sum(pop))

growth<-left_join(growth, pop2020, by=c("intervention", "wb2021"))
growth$ageprop<-growth$pop/growth$totalpop2020
sum(growth$ageprop[growth$cause=="ihd"])

growth<-left_join(growth, pop2050, by=c("intervention", "wb2021"))
growth$newpop<-growth$ageprop*growth$totalpop2050

growth$deathsexpgrowth<-growth$newpop*growth$deathrt
growth<-growth%>%group_by(cause, intervention, wb2021)%>%summarise(growthdeaths=sum(deathsexpgrowth))

output<-left_join(output, growth, by=c("cause", "intervention", "wb2021"))
output$change_growth<-((output$growthdeaths-output$deaths2020)/output$deaths2020)*100

##Scenario 2: use 2050 pop and age structure with 2020 death rates
aging<-df%>%filter(year==2020, cause=="ihd")
aging<-left_join(deaths, aging%>%dplyr::select(age, pop, intervention, wb2021), by=c("age", "intervention", "wb2021", "year"))
aging<-aging%>%dplyr::select(-c(cause.y, year))%>%rename(cause = cause.x)

aging$deathrt<-aging$causedeath/aging$pop

aging<-left_join(aging%>%dplyr::select(-c(pop, year)), 
                 df%>%filter(cause=="ihd", year==2050)%>%
                   group_by(age, intervention, wb2021)%>%
                   summarise(pop=sum(pop)), 
                 by=c("age", "intervention", "wb2021")) 

#aging<-aging%>%select(-c(cause.y, year))%>%rename(cause = cause.x)

aging$agedeaths<-aging$pop*aging$deathrt
aging<-aging%>%group_by(cause, intervention, wb2021)%>%summarise(agedeaths=sum(agedeaths))

output<-left_join(output, aging, by=c("cause", "intervention", "wb2021"))
output$change_age<-(((output$agedeaths-output$deaths2020)/output$deaths2020)*100)-output$change_growth

output$change_epi<-output$change-output$change_age-output$change_growth

names(output)[6]<-"Percent change in deaths (2020-2050)"
names(output)[7]<-"Deaths expected using 2050 total pop, 2020 pop age structure, 2020 age- and sex-specific mortality rates"
names(output)[8]<-"Percent change from 2020 due to growth"
names(output)[9]<-"Deaths expected using 2050 total pop, 2050 pop age structure, 2020 age- and sex-specific mortality rates"
names(output)[10]<-"Percent change from 2020 due to aging"
names(output)[11]<-"Percent change from 2020 due to age-specific mortality rates"


#setwd("~/RTSL/web_appendix")
#write.csv(output, "decomp_data_BG.csv", row.names = F)

### graph time! ###

plot<-output[,c(1,2,3,6,8,10,11)]
names(plot)[4]<-"Net change in CVD deaths"
names(plot)[5]<-"Change due to population growth"
names(plot)[6]<-"Change due to population aging"
names(plot)[7]<-"Change due to age-specific CVD mortality rates"


plot<-plot%>%mutate(intervention = ifelse(intervention=="Progress_Both", "Progress",
                                          ifelse(intervention=="Aspirational_Both", "Aspirational", intervention)))

plot$intervention2<-factor(plot$intervention,levels = c("Business as usual", "Progress", "Progress_Salt reduction",
                                                        "Progress_Antihypertensive therapy", "Aspirational_Salt reduction",
                                                        "Aspirational_Antihypertensive therapy","Aspirational"))

plot<-plot%>%filter(cause=="all cvd")%>%
  gather(metric, change, -wb2021, -intervention, -cause, -`Net change in CVD deaths`, -intervention2)

plot$metric<-factor(plot$metric,levels = c("Change due to population aging", 
                                           "Change due to population growth", 
                                           "Change due to age-specific CVD mortality rates"))


plot$wb2021<-factor(plot$wb2021, levels = c("All countries",
                                            "LIC",
                                            "LMIC",
                                            "UMIC", "HIC"))
library(ggthemes)

p<-ggplot(plot%>%filter(intervention2%in%c("Business as usual", "Progress", "Aspirational")), 
          aes(x=intervention2))+
  geom_bar(stat='identity', aes(y=change, fill=metric), position="stack")+
  geom_point(aes(y=`Net change in CVD deaths`,shape = "Net change"))+
  facet_grid(~wb2021, labeller = labeller(wb2021= label_wrap_gen(width=16)))+
  theme_calc()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="bottom", legend.box = "horizontal", legend.direction = "horizontal")+
  scale_fill_manual(values = c("#56B4E9","#E69F00","#009E73"))+
  #theme_calc()+
  xlab("")+
  ylab("Change in CVD Deaths 2020 to 2050 (%)")+
  labs(fill="", shape=NULL)+
  ylim(c(-175,300))

p

ggsave("../output/fig_A1.pdf", width=11, height=6, dpi=600)
write.csv(plot, "../output/decomp_data_deaths2022.csv")


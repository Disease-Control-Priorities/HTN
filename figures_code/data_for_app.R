##pull data for online appendix

setwd("~/RTSL/model")

library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)

intervention_out<-read.csv("intervention_out_ref_2022.csv", stringsAsFactors = F)
intervention_out2<-read.csv("intervention_out_asp_2022.csv", stringsAsFactors = F)

bau<-intervention_out%>%filter(intervention=="b.a.u")%>%mutate(intervention = "NA", scenario = "Business as usual")
prog<-intervention_out%>%filter(intervention!="b.a.u")
asp<-intervention_out2%>%filter(intervention!="b.a.u")

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

for (c in countrylist){
  out<-bind_rows(bau%>%filter(location==c),
                 prog%>%filter(location==c)%>%mutate(scenario="Progress"),
                 asp%>%filter(location==c)%>%mutate(scenario="Aspirational"))
  write.csv(out%>%
              group_by(location, intervention, scenario, 
                       cause, year, sex)%>%
              summarise(`CVD deaths` = sum(dead),
                        `All deaths` = sum(all.mx),
                        `New cases` = sum(newcases),
                        `Prevalent cases` = sum(sick),
                        `Population` = sum(pop)
                        )%>%
              filter(year>=2020),
              paste0("../BP_Targets/data/", c, ".csv"), 
            row.names = F)
}  



# + Global #
out<-bind_rows(bau,
               prog%>%mutate(scenario="Progress"),
               asp%>%mutate(scenario="Aspirational"))

write.csv(out%>%
            mutate(location="Global")%>%
            group_by(intervention, scenario, 
                     cause, year, sex)%>%
            summarise(`CVD deaths` = sum(dead),
                      `All deaths` = sum(all.mx),
                      `New cases` = sum(newcases),
                      `Prevalent cases` = sum(sick),
                      `Population` = sum(pop)
            )%>%
            filter(year>=2020),
          "../BP_Targets/data/Global.csv", 
          row.names = F)



#50q30

#decomp#
int1<-intervention_out%>%filter(intervention=="b.a.u")%>%select(-intervention)%>%
  mutate(scenario = "Business as usual")
int2<-intervention_out%>%filter(intervention =="Both")%>%select(-intervention)%>%
  mutate(scenario = "Progress")
in32<-intervention_out2%>%filter(intervention =="Both")%>%select(-intervention)%>%
  mutate(scenario = "Aspirational")

comb<-bind_rows(int1, int2, in32)%>%select(location, age, year, scenario, pop, newcases)
all<-comb%>%group_by(age, year, scenario)%>%
  summarise(pop=sum(pop), newcases=sum(newcases))%>%
  mutate(location="Global")
comb<-bind_rows(comb,all)

p20<-comb%>%
  filter(year==2020)%>%
  group_by(location, age, year, scenario)%>%
  summarise(pop=sum(pop)/4,  
            newcases=sum(newcases))%>%
  ungroup()%>%
  select(-year)%>%
  rename(pop20 = pop, cases20 = newcases)

p50<-comb%>%
  filter(year==2050)%>%
  group_by(location, age, year, scenario)%>%
  summarise(pop=sum(pop)/4,
            newcases=sum(newcases))%>%
  ungroup()%>%
  select(-year)%>%
  rename(pop50 = pop, cases50 = newcases)

p<-left_join(p20, p50, by=c("scenario", "age", "location"))

p$cases20[p$cases20<1]<-1
p$cases50[p$cases50<1]<-1
p$pop20[p$pop20<1]<-1
p$pop50[p$pop50<1]<-1

ptest<-p%>%mutate(change = 100*(cases50 - cases20)/cases20,
              IR20 = cases20/pop20,
              IR50 = cases50/pop50)%>%
  group_by(scenario, location)%>%
  mutate(totalpop20 = sum(pop20),
         totalpop50 = sum(pop50))%>%
  ungroup()%>%
  mutate(growth = (pop20/totalpop50)*IR20,
         change_growth = 100*(growth - cases20)/cases20,
         aging = pop50*IR20,
         change_age = 100*(aging - cases20)/cases20,
         change_epi = change - change_growth - change_age)%>%
  mutate(scenario = factor(scenario, levels=c("Business as usual",
                                              "Progress",
                                              "Aspirational")))%>%
  group_by(scenario, location)%>%
  summarise(change = sum(change), change_growth = sum(change_growth),
            change_age = sum(change_age), change_epi = sum(change_epi))%>%
  ungroup()%>%
  rename("Net change in new cases" = change,
         "Change due to population growth" = change_growth,
         "Change due to population aging" = change_age,
         "Change due to age-specific CVD incidence rates" = change_epi)%>%
  gather(metric, change, -`Net change in new cases`, -scenario, -location)%>%
  mutate(metric = factor(metric, levels = c("Change due to population aging", 
                                            "Change due to population growth", 
                                            "Change due to age-specific CVD incidence rates")))


ggplot(ptest%>%filter(location=="Global"), aes(x=scenario))+
  geom_bar(stat='identity', aes(y=change/100, fill=metric), position="stack")+
  geom_point(aes(y=`Net change in new cases`/100,shape = "Net change"))+
  theme_calc()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #theme(legend.position="bottom", legend.box = "horizontal", legend.direction = "horizontal")+
  scale_fill_manual(values = c("#56B4E9","#E69F00","#009E73"))+
  #theme_calc()+
  xlab("")+
  ylab("Change in incidence 2020 to 2050 (%)")+
  labs(fill="", shape=NULL)

#WPP#

model.pop<-read.csv("../web_appendix/shiny/pop.csv", stringsAsFactors = F)
wpp.pop<-read.csv("../web_appendix/shiny/wpp.csv", stringsAsFactors = F)
regions<-read.csv("../web_appendix/shiny/regions.csv", stringsAsFactors = F)

plot3<-bind_rows(model.pop%>%filter(age.group!="95-99")%>%
                   group_by(year, source, age.group, location)%>%summarise(pop=sum(pop)), 
                 wpp.pop%>%filter(year<=2055, substring(age.group,1,1)>=2, 
                                  age.group!="95-99", age.group!="5-9")%>%
                   group_by(year, source, age.group, location)%>%summarise(pop=sum(pop)*1000))%>%spread(source, pop)

ggplot(plot3%>%filter(location=="Georgia"), aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

plot3<-plot3%>%filter(location%in%countrylist)

all<-plot3%>%group_by(year, age.group)%>%
  summarise(model = sum(model),
            WPP=sum(WPP))%>%mutate(location="Global")

ggplot(all, aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

wpp.plot<-bind_rows(plot3, all)
write.csv(wpp.plot, "wpp_data.csv", row.names = F)


ggplot(wpp.plot%>%filter(location=="China"), aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

##gbd
plot4<-read.csv("../web_appendix/shiny/total_deaths.csv", stringsAsFactors =  F)

ggplot(plot4%>%filter(location=="China"), aes(x=year, y=Deaths/1e6, color=Scenario))+
  geom_line(size=1)+
  facet_wrap(~cause)+
  ylab("Deaths (millions)")+
  xlab("Year")

all<-plot4%>%group_by(year, cause, Scenario)%>%
  summarise(Deaths = sum(Deaths))%>%
  mutate(location="Global", wb2021="Global")

gbd<-bind_rows(plot4, all)

write.csv(gbd, "gbd_deaths.csv", row.names = F)

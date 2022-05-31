setwd("~/RTSL/model")

library(dplyr)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(tidyr)

ref<-read.csv("intervention_out_ref_2022.csv", stringsAsFactors = F)
asp<-read.csv("intervention_out_asp_2022.csv", stringsAsFactors = F)

#ref<-intervention_out
#asp<-intervention_out2

#ideal<-read.csv("intervention_out_ideal_2021_new.csv", stringsAsFactors = F)

#something weird happening with only salt run...
DA_ref<-ref%>%group_by(intervention)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA_drug = `b.a.u` - `Antihypertensive therapy`,
         DA_salt = `b.a.u` - `Salt reduction`,
         DA_both = `b.a.u`- Both)
DA_ref$DA_drug/DA_ref$DA_both
DA_ref$DA_salt/DA_ref$DA_both


DA_asp<-asp%>%group_by(intervention)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA_drug = `b.a.u` - `Antihypertensive therapy`,
         DA_salt = `b.a.u` - `Salt reduction`,
         DA_both = `b.a.u`- Both)

DA_asp$DA_drug/DA_asp$DA_both
DA_asp$DA_salt/DA_asp$DA_both


DA_asp<-asp%>%filter(age<40)%>%group_by(intervention)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA_drug = `b.a.u` - `Antihypertensive therapy`,
         DA_salt = `b.a.u` - `Salt reduction`,
         DA_both = `b.a.u`- Both)

DA_asp$DA_drug/DA_asp$DA_both


region<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, Super_region)

asp<-left_join(asp, region%>%rename(location = location_gbd))

DA_cause<-asp%>%group_by(intervention, cause, Super_region)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA_drug = `b.a.u` - `Antihypertensive therapy`,
         DA_salt = `b.a.u` - `Salt reduction`,
         DA_both = `b.a.u`- Both)

ggplot(DA_cause, aes(x=Super_region, y=DA_both, fill=cause))+
  geom_bar(position = 'dodge', stat = 'identity')


DA_cause2<-asp%>%group_by(intervention, Super_region)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA_drug = `b.a.u` - `Antihypertensive therapy`,
         DA_salt = `b.a.u` - `Salt reduction`,
         All = `b.a.u`- Both)
p2<-DA_cause%>%select(DA_both, cause, Super_region)%>%
  spread(cause, DA_both)
p2<-left_join(p2, DA_cause2%>%select(All,Super_region))

p2<-p2%>%mutate(ihd = ihd/All,
                istroke = istroke/All,
                hstroke = hstroke/All,
                hhd = hhd/All)%>%
  gather(cause, DA,  -Super_region)


ggplot(p2%>%filter(cause!="All"), aes(x=Super_region, y=DA, fill=cause))+
  geom_bar(position = 'dodge', stat = 'identity')

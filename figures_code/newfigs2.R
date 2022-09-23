rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../model/model_output_updated.Rda")
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)
library(tidyr)

#####################################
#HTN distribution inspection
#####################################

Country<-"India"

DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))

test<-get.bp.prob(DT.in, 0, 'percent', 2023, 2030, 1, "p975")

htn<-test%>%filter(bp_cat%in%c("<120", "120-129","130-139"))%>%
  group_by(Year, age, sex)%>%
  summarise(htn = 1-sum(prob))

ggplot(htn%>%filter(age=="50-54"),aes(x=Year, y=htn, color=sex))+
  geom_point()

ggplot(htn%>%filter(Year==2019 | Year==2025| Year==2030),
       aes(x=age, y=htn, color=Year))+
  facet_wrap(~sex)+
  geom_point()


DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))

test2<-get.bp.prob(DT.in, 0.3, 'percent', 2023, 2027, 1, "baseline")

htn2<-test2%>%filter(bp_cat%in%c("<120", "120-129","130-139"))%>%
  group_by(Year, age, sex)%>%
  summarise(htn = 1-sum(prob))

ggplot(htn2%>%filter(age=="50-54"),aes(x=Year, y=htn, color=sex))+
  geom_point()

ggplot(htn2%>%filter(Year==2019 | Year==2025| Year==2030),
       aes(x=age, y=htn, color=Year))+
  facet_wrap(~sex)+
  geom_point()

plot<-bind_rows(htn%>%mutate(intervention = "treatment"),
                htn2%>%mutate(intervention = "salt"),
                htn2%>%filter(Year==2019)%>%mutate(intervention="baseline", Year=2025),
                htn2%>%filter(Year==2019)%>%mutate(intervention="baseline", Year=2030))

ggplot(plot%>%filter(Year==2025| Year==2030),
       aes(x=age, y=htn, color=intervention))+
  facet_grid(Year~sex)+
  geom_point()

#ggsave("bp_diffs_india.png", height=8, width=10)

#######################################################
#residual risk example
######################################################

china<-project.all("China", 'percent', 0, 2025, "ideal")

plot<-china%>%group_by(location, year, cause, intervention)%>%
  summarise(dead=sum(dead),
            sick = sum(newcases))

ggplot(plot%>%filter(location=="China", intervention%in%c("Both", "b.a.u"))%>%
         mutate(intervention = ifelse(intervention=="Both", "100% HTN control", "Baseline")), 
       aes(x=year, y=dead, color=intervention))+
  geom_line()+
  facet_wrap(~cause)+
  ylim(0,6e6)+
  ylab("Deaths (counts)")+
  xlab("Year")+
  theme_bw()

##Cohort of 60-64
china%>%filter(cause=='ihd', year==2023, age>=60, age<=64)%>%
  group_by(intervention, year)%>%
  summarise(incidence = 100000*sum(newcases)/sum(pop))

######################################
## HHD deaths
######################################

p<-progress%>%
  group_by(year, cause, intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA = `b.a.u` - Both)%>%
  group_by(cause)%>%
  summarise(DA=sum(DA))
  
p$DA[p$cause=='hhd']/sum(p$DA)

p2<-progress%>%
  group_by(year, cause, intervention)%>%
  summarise(sick=sum(newcases))%>%
  spread(intervention, sick)%>%
  mutate(DA = `b.a.u` - Both)%>%
  group_by(cause)%>%
  summarise(DA=sum(DA))

p2$DA[p2$cause=='hhd']/sum(p2$DA)


####################################
# benefits by intervention
unique(progress$intervention)

p<-progress%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DAboth = `b.a.u` - Both,
         DAsalt = `b.a.u` - `Salt reduction`,
         DAdrug = `b.a.u` - `Antihypertensive therapy`)

p[6]/(p[6]+p[7])
p[7]/(p[6]+p[7])

p<-aspirational%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DAboth = `b.a.u` - Both,
         DAsalt = `b.a.u` - `Salt reduction`,
         DAdrug = `b.a.u` - `Antihypertensive therapy`)

p[6]/(p[6]+p[7])
p[7]/(p[6]+p[7])


#################################
#2040 numbers
progress%>%
  filter(year<=2040)%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DAboth = `b.a.u` - Both)

aspirational%>%
  filter(year<=2040)%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DAboth = `b.a.u` - Both)

#####################################
#disease free survival figure 2
#####################################
out.bp<-data.frame(age = character(),
                   sex = character(),
                   Year = numeric(),
                   bp_cat = character(),
                   prob = numeric(),
                   location = character()
  
)


for(is in countrylist){
  DT<-unique(data.in[location==is][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  temp<-get.bp.prob(DT.in, 0.15, 'percent', 2023, 2030, 1, "p75")
  out.bp<-bind_rows(out.bp, temp)
}

htn<-out.bp%>%filter(bp_cat%in%c("<120", "120-129","130-139"))%>%
  group_by(Year, age, sex, location)%>%
  summarise(htn = 1-sum(prob))

htn<-data.table(htn)

#duplicating data to be single-year-age-specific
htn[, age:=as.numeric(substr(age, 1,2))]
htn<-htn[rep(seq_len(173264), each=5)]
htn[,age2:=rep(1:5, 173264)][,age:=age+age2-1]

over90<-htn[age==89]

over90<-over90[rep(seq_len(nrow(over90)), each=6)]
over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]

#bind  
htn<-rbindlist(list(htn, over90))
#save
phtn<-htn%>%mutate(Scenario = "Progress")%>%select(-age2)  

######asp
out.bp<-data.frame(age = character(),
                   sex = character(),
                   Year = numeric(),
                   bp_cat = character(),
                   prob = numeric(),
                   location = character()
                   
)

for(is in countrylist){
  DT<-unique(data.in[location==is][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  temp<-get.bp.prob(DT.in, 0.3, 'percent', 2023, 2027, 1, "p975")
  out.bp<-bind_rows(out.bp, temp)
}

htn<-out.bp%>%filter(bp_cat%in%c("<120", "120-129","130-139"))%>%
  group_by(Year, age, sex, location)%>%
  summarise(htn = 1-sum(prob))

htn<-data.table(htn)

#duplicating data to be single-year-age-specific
htn[, age:=as.numeric(substr(age, 1,2))]
htn<-htn[rep(seq_len(173264), each=5)]
htn[,age2:=rep(1:5, 173264)][,age:=age+age2-1]

over90<-htn[age==89]

over90<-over90[rep(seq_len(nrow(over90)), each=6)]
over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]

#bind  
htn<-rbindlist(list(htn, over90))
#save
ahtn<-htn%>%mutate(Scenario = "Aspirational")%>%select(-age2)  


######baseline
out.bp<-data.frame(age = character(),
                   sex = character(),
                   Year = numeric(),
                   bp_cat = character(),
                   prob = numeric(),
                   location = character()
                   
)

for(is in countrylist){
  DT<-unique(data.in[location==is][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  temp<-get.bp.prob(DT.in, 0, 'percent', 2023, 2027, 1, "baseline")
  out.bp<-bind_rows(out.bp, temp)
}

htn<-out.bp%>%filter(bp_cat%in%c("<120", "120-129","130-139"))%>%
  group_by(Year, age, sex, location)%>%
  summarise(htn = 1-sum(prob))

htn<-data.table(htn)

#duplicating data to be single-year-age-specific
htn[, age:=as.numeric(substr(age, 1,2))]
htn<-htn[rep(seq_len(173264), each=5)]
htn[,age2:=rep(1:5, 173264)][,age:=age+age2-1]

over90<-htn[age==89]

over90<-over90[rep(seq_len(nrow(over90)), each=6)]
over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]

#bind  
htn<-rbindlist(list(htn, over90))
#save
bhtn<-htn%>%mutate(Scenario = "Business as usual")%>%select(-age2)  


output<-bind_rows(progress%>%filter(intervention=="Both")%>%mutate(Scenario = "Progress"),
                  aspirational%>%filter(intervention=="Both")%>%mutate(Scenario = "Aspirational"),
                  progress%>%filter(intervention=="b.a.u")%>%mutate(Scenario = "Business as usual"))

htn<-bind_rows(ahtn, bhtn, phtn)%>%rename(year=Year)


df<-left_join(output, htn)

#pull cohort
plot<-data.frame(year = numeric(),
                 Scenario = character(),
                 disease_free = numeric())

unique(df$intervention)

cohort_pop<-df%>%filter(age>=35, age<=39, year==2020)%>%
  filter(cause=='ihd', Scenario=="Business as usual", intervention=="b.a.u")%>%
  summarise(pop = sum(pop))%>%pull(pop)

for(i in 1:31){
  
  temp<-df%>%
    filter(age>=34+i, age<=38+i, year==2019+i)%>%
    group_by(year, Scenario)%>%
    summarise(disease_free = sum(((pop/4)-sick-(all.mx/4))*(1-htn))/cohort_pop)  #need to pull out pop as a constant**
  
  plot<-bind_rows(temp, plot)
}

##All age, all county, disease-free survival for a cohort
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
plot$Scenario<-factor(plot$Scenario, levels=c("Aspirational", "Progress", "Business as usual"))

ggplot(plot, aes(x=year, y=100*disease_free, color=Scenario, group=Scenario))+
         geom_line(size=1)+
  ylab("Proportion free of hypertension, CVD, or deaths from any cause (%)")+
  xlab("Year")+
  scale_colour_manual(values=cbPalette[c(3,2,4)])+
  theme_bw()+
  ylim(0,100)

ggsave("../output/fig_1_updated.pdf", width = 10, height=8, dpi=600)

plot%>%filter(year==2050)


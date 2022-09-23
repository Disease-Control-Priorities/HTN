#################################################################################################
#################################################################################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#################################################################################################
#gbd 2019 data#
dt1<-fread("gbd1.csv")
dt2<-fread("gbd2.csv")
dt3<-fread("gbd3.csv")
dt4<-fread("gbd4.csv")
dt5<-fread("gbd5.csv")
dt6<-fread("gbd6.csv")
dt7<-fread("gbd7.csv")
dt8<-fread("gbd8.csv")

dt<-rbindlist(list(dt1,dt2,dt3,dt4,dt5,dt6,dt7,dt8))
dt[, upper:=NULL]
dt[, lower:=NULL]

## update GBD names ##
iso3<-fread("Country_groupings_extended.csv")
iso3<-iso3[, c("gbd2019", "location_gbd")]
setnames(iso3, "gbd2019", "location")
dt<-merge(dt, iso3, by="location")
dt[,location:=NULL]
setnames(dt, "location_gbd", "location")

#using GBD 2019 population estimates
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
                       pop15,pop16,pop17,pop18,pop19))%>%filter(sex_name!="both" & location_id!=533) #Georgia the state

gbdpop$age_group<-as.numeric(gbdpop$age_group_name)
gbdpop$age_group[gbdpop$age_group_name=="<1 year"]<-0
gbdpop$age_group[gbdpop$age_group_name=="95 plus"]<-95

names(gbdpop)[2]<-"location"
totalpop<-merge(gbdpop, iso3, by="location")
totalpop[,location:=NULL]

totalpop$sex_name[totalpop$sex_name=="male"]<-"Male"
totalpop$sex_name[totalpop$sex_name=="female"]<-"Female"

#################################################################################################
# As a function
#################################################################################################

project.all <- function(Country, yr){
  
  gbd_data<-dt[year==yr]
  IHME_DEMO<-totalpop%>%filter(year_id==yr)

#################################################################################################
pop.df <- IHME_DEMO %>% rename(location = location_gbd, sex = sex_name, age = age_group, Nx=val) %>%
  select(location, sex, age, Nx) %>% filter(location==Country & age > 19)

#################################################################################################
# function to get the rates

other.rates <- function(met, meas, name, sel){
  df    <- gbd_data %>% filter(metric==met & measure==meas & location==Country) %>%
    mutate(midptage = as.numeric(substr(age,1,2)) + 2) %>% arrange(sex, cause, midptage) %>%
    select(sex, midptage, val, cause) %>% spread(cause, val) %>% 
    mutate(ihd     = `All causes` - `Ischemic heart disease`,  istroke = `All causes` - `Ischemic stroke`, 
           hstroke = `All causes` - `Intracerebral hemorrhage`,hhd     = `All causes` - `Hypertensive heart disease`) 
  
  if (sel == 1){df  <- df %>%  mutate(ihd = `Ischemic heart disease`, istroke = `Ischemic stroke`, 
                                      hstroke = `Intracerebral hemorrhage`, hhd = `Hypertensive heart disease`, all=`All causes`)}
  
  df   <- df %>% select(sex, midptage, ihd, istroke, hstroke, hhd, all) %>% arrange(sex, midptage)
  
  interpolate.rate <- function(y){approx(c(seq(22,92,5), 95) ,y, xout=20:95, rule=2, method="linear")$y}
  
  df.rates <- df %>% filter(sex=="Female") %>% select(ihd, istroke, hstroke, hhd, all) %>% as.matrix() %>%
    apply(., 2, interpolate.rate) %>% data.table() %>% mutate(sex = "Female", age = 20:95)
  df.rates <- df %>% filter(sex=="Male") %>% select(ihd, istroke, hstroke, hhd, all) %>% as.matrix() %>%
    apply(., 2, interpolate.rate) %>% data.table() %>% mutate(sex = "Male", age = 20:95) %>%
    rbind(df.rates) %>% gather(cause, val, -sex, -age) %>% mutate(val = val/100000, location = Country, year = yr)
  setnames(df.rates, "val", name)
  df.rates
}


#################################################################################################
# Background rates

prev.rates  <- other.rates("Rate","Prevalence","PREVt0", 1)
death.rates <- other.rates("Rate","Deaths","DIS.mx.t0", 1)

bg.rates<-death.rates%>%spread(cause, DIS.mx.t0)
bg.rates$BG.mx.all<-bg.rates$all-(bg.rates$ihd+ bg.rates$hhd+bg.rates$istroke+bg.rates$hstroke)
bg.rates$BG.mx.ihd<-bg.rates$all-bg.rates$ihd
bg.rates$BG.mx.hhd<-bg.rates$all-bg.rates$hhd
bg.rates$BG.mx.istroke<-bg.rates$all-bg.rates$istroke
bg.rates$BG.mx.hstroke<-bg.rates$all-bg.rates$hstroke

bg.rates<-bg.rates%>%select(-c(hhd, ihd, istroke, hstroke))%>%
  gather(cause, BG.mx, -BG.mx.all, -age, -sex, -location, -year, -all)

bg.rates$cause[bg.rates$cause=="BG.mx.ihd"]<-"ihd"
bg.rates$cause[bg.rates$cause=="BG.mx.hhd"]<-"hhd"
bg.rates$cause[bg.rates$cause=="BG.mx.istroke"]<-"istroke"
bg.rates$cause[bg.rates$cause=="BG.mx.hstroke"]<-"hstroke"
names(bg.rates)[5]<-"ALL.mx"


#################################################################################################

jvars          <- c("age","sex","location","year")

#################################################################################################

baseline_rates<-
  left_join(bg.rates, prev.rates, by = c(jvars, "cause")) %>% # background + prev
  left_join(death.rates, by = c(jvars, "cause")) %>% # death rates
  left_join(pop.df, by = c("location", "sex", "age")) %>%
  rename(Nx = Nx) %>% arrange(sex, cause, age)


#################

baseline_rates$location <- Country

baseline_rates

}

#################################################################################################

countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global")%>%pull(location)

data.out<-project.all("Afghanistan", 2000)

for (i in 2:195) {
  dat <- project.all(countrylist[i], 2000)
  data.out<-bind_rows(dat,data.out)

}

any(is.na(data.out))

for (i in 1:195){
for(j in 1:19){
dat<- project.all(countrylist[i], 2000+j)
data.out<-bind_rows(dat, data.out)
  }
}

write.csv(data.out, "baseline_rates_noIR2019.csv", row.names = FALSE)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)

load("../model/model_output_newnorm.Rda")

comb<-bind_rows(progress%>%filter(intervention!="b.a.u")%>%
                  mutate(intervention = paste0("Progress_", intervention)),
                aspirational%>%filter(intervention!='b.a.u')%>%
                  mutate(intervention = paste0("Aspirational_", intervention)),
                progress%>%filter(intervention=='b.a.u')%>%
                  mutate(intervention = "Business as usual")
                )

##currently excluding the 95+ age group 
base<-comb%>%filter(year==2020 | year==2030 | year==2040 | year==2050)%>%
  filter(age!=95, cause=="ihd")

base$AgeGrp[base$age<25 & base$age>=20]<-"20-24"
base$AgeGrp[base$age<30 & base$age>=25]<-"25-29"
base$AgeGrp[base$age<35 & base$age>=30]<-"30-34"
base$AgeGrp[base$age<40 & base$age>=35]<-"35-39"
base$AgeGrp[base$age<45 & base$age>=40]<-"40-44"
base$AgeGrp[base$age<50 & base$age>=45]<-"45-49"
base$AgeGrp[base$age<55 & base$age>=50]<-"50-54"
base$AgeGrp[base$age<60 & base$age>=55]<-"55-59"
base$AgeGrp[base$age<65 & base$age>=60]<-"60-64"
base$AgeGrp[base$age<70 & base$age>=65]<-"65-69"
base$AgeGrp[base$age<75 & base$age>=70]<-"70-74"
base$AgeGrp[base$age<80 & base$age>=75]<-"75-79"
base$AgeGrp[base$age<85 & base$age>=80]<-"80-84"
base$AgeGrp[base$age<90 & base$age>=85]<-"85-89"
base$AgeGrp[base$age<95 & base$age>=90]<-"90-94"
#base$AgeGrp[base$age>=95]<-"95+"

base<-base%>%group_by(AgeGrp, sex, location, year, intervention)%>%
  summarize(pop=sum(pop), deaths=sum(all.mx))
names(base)
names(base)[2]<-"Sex"
names(base)[3]<-"Location"
names(base)[4]<-"Time2"


#combining with WPP estimates for ages 0-19 (as we don't model these)
WPP_LT <- read.csv("../model/WPP_LT2.csv", stringsAsFactors=FALSE)%>%filter(AgeGrpStart<20)%>%
  select(Sex, Location, AgeGrp, Time2, pop, deaths)

codes <- read.csv("../model/Country_groupings_extended.csv", stringsAsFactors=FALSE)
names(codes)
names(codes)[6]<-"Location"
countrylist <- read.csv("../model/super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global", 
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
                                                                              location!="Virgin Islands, U.S.")%>%pull(location)

unique(WPP_LT$Location)

WPP_LT<-left_join(WPP_LT, codes%>%select(location_gbd, Location, LocID), by="Location")%>%
  select(-c(Location, LocID))%>%
  rename(Location = location_gbd)%>%
  filter(Location%in%countrylist)

unique(WPP_LT$Location)
unique(comb$intervention)

lifetab<-bind_rows(WPP_LT%>%mutate(intervention="Business as usual"),
                   WPP_LT%>%mutate(intervention="Progress_Antihypertensive therapy"),
                   WPP_LT%>%mutate(intervention="Progress_Salt reduction"),
                   WPP_LT%>%mutate(intervention="Progress_Both"),
                   WPP_LT%>%mutate(intervention="Aspirational_Antihypertensive therapy"),
                   WPP_LT%>%mutate(intervention="Aspirational_Salt reduction"),
                   WPP_LT%>%mutate(intervention="Aspirational_Both"),
                   base)

#adding calculations for both sexes, and all countries
bothsex<-lifetab%>%group_by(AgeGrp, Time2, Location, intervention)%>%summarize(pop=sum(pop), deaths=sum(deaths))
bothsex$Sex<-"Both"
lifetab<-bind_rows(lifetab, bothsex)
allcountry<-lifetab%>%group_by(AgeGrp, Time2, Sex, intervention)%>%summarize(pop=sum(pop), deaths=sum(deaths))
allcountry$Location<-"All countries"
lifetab<-bind_rows(lifetab, allcountry)


##setting Age Group Starts for the added WPP values
lifetab$AgeGrpStart<-as.numeric(substr(lifetab$AgeGrp, 1,2))
lifetab$AgeGrpStart[lifetab$AgeGrp=="0"]<-0
lifetab$AgeGrpStart[lifetab$AgeGrp=="1-4"]<-1
lifetab$AgeGrpStart[lifetab$AgeGrp=="5-9"]<-5

lifetab$n<-5
lifetab$n[lifetab$AgeGrp=="0"]<-1
lifetab$n[lifetab$AgeGrp=="1-4"]<-4

lifetab$Mx<-lifetab$deaths/lifetab$pop

#test if any 1m0 >=0.107
max(lifetab$Mx[lifetab$AgeGrp==0])

lifetab$ax<-2.5
lifetab$ax[lifetab$AgeGrp=="0" & lifetab$Sex=="Female"]<-0.053+(2.8*lifetab$Mx[lifetab$AgeGrp=="0"& lifetab$Sex=="Female"])
lifetab$ax[lifetab$AgeGrp=="1-4"& lifetab$Sex=="Female"]<-1.522-(1.518*lifetab$Mx[lifetab$AgeGrp=="0"& lifetab$Sex=="Female"])
lifetab$ax[lifetab$AgeGrp=="0" & lifetab$Sex=="Male"]<-0.045+(2.684*lifetab$Mx[lifetab$AgeGrp=="0"& lifetab$Sex=="Male"])
lifetab$ax[lifetab$AgeGrp=="1-4"& lifetab$Sex=="Male"]<-1.651-(2.816*lifetab$Mx[lifetab$AgeGrp=="0"& lifetab$Sex=="Male"])

lifetab$qx<-(lifetab$n*lifetab$Mx)/(1+(lifetab$n-lifetab$ax)*lifetab$Mx)
lifetab$qx[lifetab$AgeGrp=="95+"]<-1

#adding index variable for sequential ages
lifetab$count<-(lifetab$AgeGrpStart/5)+2
lifetab$count[lifetab$AgeGrpStart==0]<-1
lifetab$count[lifetab$AgeGrpStart==1]<-2

lifetab$lx[lifetab$AgeGrpStart==0]<-100000

##############################################################################################
#switch to using DT for speed (just learned about this package)
#calc lx
##############################################################################################

lt1<-as.data.table(lifetab%>%filter(Location!="South Sudan"))#issue with SSD. need to investigate.
lt1<-lt1[order(Sex, Location, Time2, intervention, count)]
unique(lt1$Location)

for(i in 1:19){
lt2<-lt1[count==i,]
lt2[,lx0:=lx*(1-qx)]
lt2[,count:=count+1]
lt2<-lt2[,c("AgeGrp", "Sex", "Location", "count", "Time2", "intervention", "lx0")]
lt1[count==i+1, lx:=lt2[,lx0]]
}

any(is.na(lt1))
################################################################################################
#calc Lx
################################################################################################
lt1[, Lx:=n*lx*(1-qx)+ax*lx*qx]

################################################################################################
#calc Tx
################################################################################################
lt1[, Tx:=sum(Lx), by=.(Sex, Location, Time2, intervention)]

for(i in 2:20){
lt2<-lt1[count>=i]
lt2[,Tx0:=sum(Lx), by=.(Sex, Location, Time2, intervention)]
lt2<-lt2[count==i, c("AgeGrp", "Sex", "Location", "count", "Time2", "intervention", "Tx0")]
lt1[count==i, Tx:=lt2[,Tx0]]
}


################################################################################################
#calc ex
################################################################################################
lt1[,ex:=Tx/lx]

##then just averaging the Male and Female values for the "Both sex" category##
lt2<-lt1[Sex=="Both"]
lt2[, exboth:=mean(ex), by=.(AgeGrp, Location, Time2, intervention)]
lt2<-lt2[Sex=="Both", c("AgeGrp", "Sex", "Location", "Time2", "intervention", "exboth")]
lt1[Sex=="Both", ex:=lt2[,exboth]]


LE<-lt1%>%filter(AgeGrp==0)%>%select(Sex, Location, ex, Time2, intervention)
which(is.na(LE))

write.csv(LE, "../output/LE0_2022.csv")

##life expectancy at 40
LE40<-lt1%>%filter(AgeGrp=="40-44")%>%select(Sex, Location, ex, Time2, intervention)
which(is.na(LE40))

write.csv(LE40, "../output/LE40_2022.csv")

####
#do for regions?
###


#################################
## deaths, cases
#################################
base2<-comb%>%filter(year%in%c(2020,2050))%>%
  filter(age!=95)%>%select(-c(well, pop))

base2<-left_join(base2, codes%>%rename(location=gbd2019)%>%select(wb2021,location))
which(is.na(base2))

alls<-base2%>%group_by(location, year, cause, age, wb2021,
                       intervention)%>%
  summarise(sick = sum(sick), newcases=sum(newcases),
            dead = sum(dead), all.mx=sum(all.mx))%>%
  mutate(sex = "Both")

base2<-bind_rows(base2, alls)

allc<-base2%>%group_by(location, year, sex, age, wb2021,
                       intervention)%>%
  summarise(sick = sum(sick), newcases=sum(newcases),
            dead = sum(dead), all.mx=sum(all.mx))%>%
  mutate(cause = "All CVD")

base2<-bind_rows(base2, allc)

base2<-base2%>%filter(year==2050 |
                        (year==2020 & intervention=="Business as usual"))

#write.csv(base2, "../web_appendix/shiny/results_2022.csv", row.names = F)
##

alldf<-comb%>%group_by(year, intervention)%>%
  summarise(
    pop=sum(pop)/4,
    sick=sum(sick),
    dead=sum(dead),
    all.mx=sum(all.mx)/4,
    newcases=sum(newcases)
  )

report<-alldf%>%filter(year%in%c(2020,2030,2040, 2050))

write.csv(report, "../output/results1_2022.csv")

#cumulative DA
cdf<-alldf%>%select(dead, year, intervention)%>%
  spread(intervention, dead)%>%
  mutate(aDA =  `Business as usual`- Aspirational_Both,
         rDA =  `Business as usual`- Progress_Both)
         #iDA =  `Business as usual` - Ideal)

write.csv(cdf, "../output/results_cumulative_2022.csv")

sum(cdf$rDA)/1e6
sum(cdf$aDA)/1e6

pcdf<-alldf%>%select(newcases, year, intervention)%>%
  spread(intervention, newcases)%>%
  mutate(aDA =  `Business as usual`-Aspirational_Both,
         rDA =  `Business as usual`- Progress_Both)
         #iDA =  `Business as usual` - Ideal)

sum(pcdf$rDA)/1e6
sum(pcdf$aDA)/1e6

write.csv(pcdf, "../output/cases_cumulative_2022.csv")


#by sex_
alldf<-comb%>%group_by(year, sex, intervention)%>%
  summarise(
    pop=sum(pop)/4,
    sick=sum(sick),
    dead=sum(dead),
    all.mx=sum(all.mx)/4,
    newcases=sum(newcases)
  )

report2<-alldf%>%filter(year%in%c(2020,2050))

write.csv(report2, "../output/results_bysex_2022.csv")
####


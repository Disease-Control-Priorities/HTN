#################################################################################################
#################################################################################################
rm(list = ls(all = TRUE)) # Clear all memory
setwd("C:/Users/sarah/Documents/RTSL/data_preprocessing")
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#################################################################################################
#data

#baseline rates calculated in file: "base_rates.R"
b_rates<-fread("base_rates_2019.csv")
b_rates[well<0, well:=0]

add50<-b_rates%>%filter(year==2040)
rep50<-add50[rep(seq(1,nrow(add50)), 10)]
rep50$year<-rep50$year+as.numeric(gl(10,110656))

b_rates<-bind_rows(b_rates, rep50)
#add age-20 cohort projections from WPP
pop20<-read.csv("PopulationsAge20_2050.csv", stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=ifelse(is.na(Nx2), pop, Nx2))%>%
  select(-c(Nx2))

#blood pressure data calculated in file: "Blood pressure.R"
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

source("functions_review_4.R")

ref<-fread("global_pop_0122.csv")

which(b_rates[,IR<0])
which(b_rates[,CF<0])
which(b_rates[, BG.mx.all<0])
which(b_rates[, BG.mx<0])
which(b_rates[, ALL.mx<0])

###fxn
repYear<-function(row){
  2017+floor((row-1)/224)
}

b_rates[, newcases:=0]

#can't have >1 for TP rows
b_rates<-b_rates%>%mutate(test = ifelse(IR+BG.mx>1,1,0))%>%
  mutate(IR2 = ifelse(test==1, (IR/(IR+BG.mx))-0.005, IR),
         BG.mx2 = ifelse(test==1, (BG.mx/(IR+BG.mx))-0.005, BG.mx))%>%
  mutate(test2=ifelse(IR2+BG.mx2>1,1,0))%>%
  mutate(test=ifelse(CF+BG.mx2>1,1,0))%>%
  mutate(CF2 = ifelse(test==1, (CF/(CF+BG.mx2))-0.005, CF),
         BG.mx3 = ifelse(test==1, BG.mx2/(CF+BG.mx2)-0.005, BG.mx2))%>%
  mutate(test3 = ifelse(CF2+BG.mx3>1,1,0))%>%
  select(-c(IR,CF,BG.mx,BG.mx2, test, test2, test3))%>%
  rename(IR = IR2, CF = CF2, BG.mx = BG.mx3)

#not adjusting IR because we're essentially doing that with bau coverage
#b_rates[year>2019, IR:=IR*(0.997)^(year-2019)]

#start CFAARC in 2017
b_rates[year<=2019, CF:=CF*CFAARC]
#adjust background mortality by 
b_rates[year>=2019, BG.mx:=BG.mx*0.985^(2020-year)]
b_rates[year>=2019, BG.mx.all:=BG.mx.all*0.985^(2020-year)]

#################################################################################################
# As a function
#################################################################################################

project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
  #Country<-"China"
  #################################################################################################
  base_rates<-b_rates[location==Country]#[, -c("year")]
 
  #################################################################################################
  #################################################################################################
  #intervention scenarios
  
  DT<-data.in[location==Country][,Year:=2017][,-c("Low95CI", "High95CI")]
  
  DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]

  bp_prob_salt<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
  bp_prob_drug<-get.bp.prob(DT.in, 0, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
  bp_prob_both<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
  bp_prob_base<-get.bp.prob(DT.in, 0, saltmet, 2023, saltyear2, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)]
  bp_prob_bau<-get.bp.prob(DT.in, 0, saltmet, 2023, saltyear2, 1, "baseline")
  
  bp_prob_salt[,intervention:="Salt reduction"]
  bp_prob_drug[,intervention:="Antihypertensive therapy"]
  bp_prob_both[,intervention:="Both"]
  bp_prob_bau[,intervention:="b.a.u"]

  
  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs<-bind_rows(bp_prob_both, bp_prob_drug, bp_prob_salt, bp_prob_bau)
  bp_probs<-merge(bp_probs, bp_prob_base, by=c("age","sex", "bp_cat", "Year", "location")) #change to "Year"
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  ##add RRis##
  addRR<-function(RR, bp){
    if(bp=="<120"){1}
    else if (bp=="120-129"){1/RR}
    else if (bp=="130-139"){1/RR^2}
    else if (bp=="140-149"){1/RR^3}
    else if (bp=="150-159"){1/RR^4}
    else if (bp=="160-169"){1/RR^5}
    else if (bp=="170-179"){1/RR^6}
    else {1/RR^7}
  }
  
  bp_probs[, RRi_IHD:=sapply(bp_cat, addRR, RR=0.83)]
  bp_probs[, RRi_HHD:=sapply(bp_cat, addRR, RR=0.72)]
  bp_probs[, RRi_stroke:=sapply(bp_cat, addRR, RR=0.73)]
 
  
  ##add alphas##
  alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                  hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD)), 
                  by=.(age, sex, location, intervention, Year)] #change to "Year"
  
  alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
       value.name="alpha")#change to "Year"
  
  
  rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]#change to "Year"
  rris[,hstroke:=RRi_stroke]
  
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd","istroke"))
  rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
               value.name="RRi")#change to "Year"
  
  bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
  setnames(bp_probs, "Year", "year")
  
  ####adding baseline_rates
  intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))
  
  #calculating yi*pi
  intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
  intervention_rates[, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                            BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"

  intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
  
 

  #################################################################################################

  ## calculate initial states for the incoming year 2000 and all years for age 20 population
  intervention_rates[year==2017 | age==20, sick:=Nx*PREVt0]
  intervention_rates[year==2017 | age==20, dead:=Nx*DIS.mx.t0]
  intervention_rates[year==2017 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
  
  
  #base_rates<-base_rates[location %in% countrylist]
  intervention_rates[age==20 | year==2017, pop:=Nx]
  intervention_rates[age==20 | year==2017, all.mx:=Nx*ALL.mx]
  
  intervention_rates[CF>0.99, CF:=0.99]
  intervention_rates[IR>0.99, IR:=0.99]
  
  #STATE TRANSITIONS#
  for(i in 1:34){
    
    b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
    b2[,age2:=age+1]
    
    #newcases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+BG.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
    b2[sick2<0, sick2:=0]
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
    #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
    b2[dead2<0, dead2:=0]
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
    #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
    b2[pop2<0, pop2:=0] #prevent negatives
    
    #all dead
    b2[, all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
    b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)]
    b2[all.mx2<0, all.mx:=0]
    
    #well
    b2[, well2:=pop2-all.mx2-sick2]
    b2[well2<0, well2:=0] #prevent negatives
    
    #re-combined into original data.table
    b2<-b2[year==2017+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", 
                                     "all.mx2", "sex", "location", "cause", "intervention")]
    setnames(b2, "age2", "age")
    intervention_rates[year==2017+i & age>20, newcases:=b2[, newcases2]]
    intervention_rates[year==2017+i & age>20, sick:=b2[,sick2]]
    intervention_rates[year==2017+i & age>20, dead:=b2[,dead2]]
    intervention_rates[year==2017+i & age>20, well:=b2[,well2]]
    intervention_rates[year==2017+i & age>20, pop:=b2[,pop2]]
    intervention_rates[year==2017+i & age>20, all.mx:=b2[,all.mx2]]
    
  }
  
  out.df<-intervention_rates[, c("age", "cause", "sex", "year", "well", "sick", "newcases",
                         "dead", "pop", "all.mx", "intervention", "location")]
  
  
}#as a fxn


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

#run model, reference scenario#

        
        intervention_out <- data.frame(age = numeric() ,
                                       sex = character(),
                                       year= numeric(),
                                       cause =  character(),
                                       well = numeric(),
                                       newcases = numeric(),
                                       sick = numeric(),
                                       dead = numeric(),
                                       pop = numeric (),
                                       all.mx = numeric(),
                                       location = character(),
                                       intervention = character(),
                                       stringsAsFactors=FALSE)

Sys.time()
for (i in 1:183) {
  # ... make some data
  dat <- project.all(countrylist[i],"percent", 0.3, 2025, "p75")
  intervention_out<-rbind(dat,intervention_out)
}
        
Sys.time() #~
write.csv(intervention_out, "intervention_out_ref_2021_alt.csv", row.names=F)
#run model, aspirational scenario#


intervention_out2 <- data.frame(age = numeric() ,
                               sex = character(),
                               year= numeric(),
                               cause =  character(),
                               well = numeric(),
                               newcases = numeric(),
                               sick = numeric(),
                               dead = numeric(),
                               pop = numeric (),
                               all.mx = numeric(),
                               location = character(),
                               intervention = character(),
                               stringsAsFactors=FALSE)

Sys.time() #~

for (i in 1:183) {
  # ... make some data
  dat <- project.all(countrylist[i],"target", 7.56, 2025, "p975")
  intervention_out2<-rbind(dat,intervention_out2)
}

write.csv(intervention_out2, "intervention_out_asp_2021_alt.csv", row.names = F)

Sys.time()
###################################################################################################

#compare population and all cause deaths
library(readxl)
malepop <- read_excel("~/RTSL/WPP2019_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx", 
                      sheet = "MEDIUM VARIANT", skip = 16, 
                      col_types = c("guess", "guess", "guess", "guess",
                                    "guess", "guess", "guess", "guess",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric"))%>%mutate(sex="Male")
femalepop <- read_excel("~/RTSL/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx", 
                      sheet = "MEDIUM VARIANT", skip = 16,
                      col_types = c("guess", "guess", "guess", "guess",
                                    "guess", "guess", "guess", "guess",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric"))%>%mutate(sex="Female")

wpp.pop<-bind_rows(malepop, femalepop)%>%rename(year = `Reference date (as of 1 July)`, 
                                            location = `Region, subregion, country or area *`)%>%
  select(-c(Index, Variant, Notes, Type, `Parent code`))%>%
  gather(age.group, pop, -location, -year, -sex, -`Country code`)%>%
  mutate(source = "WPP")

maledeaths <- read_excel("~/RTSL/WPP2019_MORT_F04_2_DEATHS_BY_AGE_MALE.xlsx", 
                      sheet = "MEDIUM VARIANT", skip = 16, 
                      col_types = c("guess", "guess", "guess", "guess",
                                    "guess", "guess", "guess", "guess",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric"))%>%
                      mutate(sex="Male")%>%rename("95-99" = `95+`)
femaledeaths <- read_excel("~/RTSL/WPP2019_MORT_F04_3_DEATHS_BY_AGE_FEMALE.xlsx", 
                        sheet = "MEDIUM VARIANT", skip = 16,
                        col_types = c("guess", "guess", "guess", "guess",
                                      "guess", "guess", "guess", "guess",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric", "numeric"))%>%
                        mutate(sex="Female")%>%rename("95-99" = `95+`)

wpp.deaths<-bind_rows(maledeaths, femaledeaths)%>%rename(location = `Region, subregion, country or area *`)%>%
  select(-c(Index, Variant, Notes, Type, `Parent code`))%>%
  gather(age.group, deaths, -location, -Period, -sex, -`Country code`)%>%
  mutate(source = "WPP")


model<-intervention_out%>%filter(intervention == "b.a.u")
## update and merge ##
model$age.group[model$age<25]<-"20-24"
model$age.group[model$age<30 & model$age>=25]<-"25-29"
model$age.group[model$age<35 & model$age>=30]<-"30-34"
model$age.group[model$age<40 & model$age>=35]<-"35-39"
model$age.group[model$age<45 & model$age>=40]<-"40-44"
model$age.group[model$age<50 & model$age>=45]<-"45-49"
model$age.group[model$age<55 & model$age>=50]<-"50-54"
model$age.group[model$age<60 & model$age>=55]<-"55-59"
model$age.group[model$age<65 & model$age>=60]<-"60-64"
model$age.group[model$age<70 & model$age>=65]<-"65-69"
model$age.group[model$age<75 & model$age>=70]<-"70-74"
model$age.group[model$age<80 & model$age>=75]<-"75-79"
model$age.group[model$age<85 & model$age>=80]<-"80-84"
model$age.group[model$age<90 & model$age>=85]<-"85-89"
model$age.group[model$age<95 & model$age>=90]<-"90-94"
model$age.group[model$age>=95]<-"95-99"

model.pop<-model%>%group_by(age.group, year, sex, location)%>%summarise(pop=sum(pop)/4)%>%
  select(c(location, year, sex, age.group, pop))%>%
  mutate(source = "model")

plot2<-bind_rows(model.pop%>%filter(age.group!="95-99")%>%
                   group_by(year, source, age.group)%>%summarise(pop=sum(pop)), 
                 wpp.pop%>%filter(year<=2055, location=="WORLD", substring(age.group,1,1)>=2, 
                                  age.group!="95-99", age.group!="5-9")%>%
                   group_by(year, source, age.group)%>%summarise(pop=sum(pop)*1000))

test<-plot2%>%spread(source, pop)

ggplot(test, aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

ggsave("world_pop.png", height=8, width=8)


plot3<-bind_rows(model.pop%>%filter(age.group!="95-99", location=="China")%>%
                   group_by(year, source, age.group)%>%summarise(pop=sum(pop)), 
                 wpp.pop%>%filter(year<=2055, location=="China", substring(age.group,1,1)>=2, 
                                  age.group!="95-99", age.group!="5-9")%>%
                   group_by(year, source, age.group)%>%summarise(pop=sum(pop)*1000))%>%spread(source, pop)

ggplot(plot3, aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

ggsave("china_pop.png", height=8, width=8)

#deaths#
intervention_out<-read.csv("~/RTSL/model/intervention_out_ref_2021.csv", stringsAsFactors = F)
outDT<-read.csv("~/RTSL/forecast/forecasts2019.csv", stringsAsFactors = F)
gbdfs<-outDT%>%group_by(year, cause, intervention, location)%>%summarise(Deaths=sum(Deaths))%>%
  rename(Scenario = intervention)
gbdfs$Scenario[gbdfs$Scenario=="gbd 2019"]<-"GBD 2019 data"
gbdfs$Scenario[gbdfs$Scenario=="GBD 2019, Reference"]<-"GBD 'Reference' projection"
gbdfs$Scenario[gbdfs$Scenario=="GBD 2019, Worse"]<-"GBD 'Worse' projection"

plot4<-bind_rows(gbdfs, intervention_out%>%filter(intervention =="b.a.u")%>%
                   group_by(cause, year, location)%>%summarise(Deaths = sum(dead))%>%
                   mutate(Scenario = "Model 'Business as usual' projection"))

plot4$cause[plot4$cause=="hhd"]<-unique(plot4$cause)[1]
plot4$cause[plot4$cause=="hstroke"]<-unique(plot4$cause)[2]
plot4$cause[plot4$cause=="ihd"]<-unique(plot4$cause)[3]
plot4$cause[plot4$cause=="istroke"]<-unique(plot4$cause)[4]

ggplot(plot4%>%filter(location=="Democratic Republic of the Congo"), aes(x=year, y=Deaths/1e6, color=Scenario))+
  geom_line(size=1)+
  facet_wrap(~cause)+
  ylab("Deaths (millions)")+
  xlab("Year")

ggsave("gbdforesight.png", height = 8, width = 8)

###########################################
############## NIXED ##############
###########################################

intervention_out3<-intervention_out%>%filter(intervention=="b.a.u")%>%mutate(intervention="Business as usual")
intervention_out<-intervention_out%>%filter(intervention=="Both")%>%mutate(intervention="Reference")
intervention_out2<-intervention_out2%>%filter(intervention=="Both")%>%mutate(intervention="Aspirational")
baseline_out$intervention<-"Baseline"
plot<-rbindlist(list(baseline_out, intervention_out, intervention_out2, intervention_out3), use.names=T)

plot$Scenario<-factor(plot$intervention, levels=c("Baseline", "Business as usual", "Reference", "Aspirational"))

library(ggthemes)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#from: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

########## FIGURE 1 ############
p1<-plot%>%group_by(year, age, Scenario)%>%summarise(Incidence=100000*(sum(newcases)/(sum(pop)/4)))

ggplot(p1%>%filter(year==2050 & Scenario!="Baseline"), aes(x=age, y=Incidence, color=Scenario))+
  geom_smooth(se=F, span=0.2)+
  ylab("New cases (per 100,000)")+
  xlab("Age (years)")+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(4,2,3,6)])+
  ggtitle("Change in CVD incidence rate by intervention scenario: 2040")

p2<-plot%>%group_by(year, age, Scenario)%>%summarise(Mx=100000*(sum(dead)/(sum(pop)/4)))

ggplot(p2%>%filter(year==2040), aes(x=age, y=Mx, color=Scenario))+
  geom_smooth(se=F, span=0.2)+
  ylab("CVD deaths (per 100,000)")+
  xlab("Age (years)")+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(4,2,3,6)])+
  ggtitle("Change in CVD mortality rate by intervention scenario: 2040")

p3<-plot%>%group_by(year, age, Scenario)%>%
  summarise(Incidence =100000*(sum(newcases)/(sum(pop)/4)),
            Deaths=100000*(sum(dead)/(sum(pop)/4)))%>%
  gather(Measure, per, -year, -age, -Scenario)

ggplot(p3%>%filter(year==2040), aes(x=age, y=per, color=Scenario))+
  geom_smooth(se=F, span=0.2, aes(linetype=Measure), lwd=1)+
  xlab("Age (years)")+
  ylab("Per 100,000 population")+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(4,2,3,6)])

ggsave("../Figure1_808080.png", height=6, width=8)



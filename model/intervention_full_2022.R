#################################################################################################
#################################################################################################
###########################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../data_preprocessing")
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#################################################################################################
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


names<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, gbd2019, iso3)%>%
  rename(location = location_gbd)

#add covid mx data
load("wpp.adj.Rda")
#Covid mx ~= excess mortality
b_rates<-fread("base_rates_2022.csv")
b_rates<-left_join(b_rates, wpp.adj%>%
                     rename(location = location_name)%>%
                     select( -Nx, -mx, -iso3))

#add location-specific CFAARC estimates
df<-read.csv("IHME-CFR.csv", stringsAsFactors = F)
cfr<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause!="All causes")%>%
  spread(year, val)%>%
  mutate(CFAARC = 1+log(`2019`/`2009`)/10)

cfr$cause[cfr$cause == "Ischemic heart disease"]<-"ihd"
cfr$cause[cfr$cause == "Hypertensive heart disease"]<-"hhd"
cfr$cause[cfr$cause == "Ischemic stroke"]<-"istroke"
cfr$cause[cfr$cause == "Intracerebral hemorrhage"]<-"hstroke"

cfr<-left_join(cfr%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`))

##add reduction in bg.mx over time
all<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause=="All causes")%>%
  spread(year, val)%>%
  mutate(BGAARC = 1+log(`2019`/`2009`)/10)

all<-left_join(all%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`, cause))

b_rates<-left_join(b_rates, all)%>%select(-c(CFAARC))
b_rates<-left_join(b_rates, cfr)

#adjust rates
b_rates[year>=2020, CF :=CF*(CFAARC^(year-2019))]
b_rates[year>=2020, BG.mx :=BG.mx*(BGAARC^(year-2019))]
b_rates[year>=2020, BG.mx.all :=BG.mx.all*(BGAARC^(year-2019))]

#add age-20 cohort projections from demographic model
pop20<-read.csv("PopulationsAge20_2050.csv", stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=ifelse(is.na(Nx2), pop, Nx2))%>%
  select(-c(Nx2))

#blood pressure data calculated in file: "Blood pressure.R"
data.in<-fread("bp_data6.csv")%>%rename(location = location_gbd)%>%select(-Year, -Country)
#data.in<-fread("bp_data3.csv")%>%select(-Year, -V1)%>%rename(Lower95 = Low95CI, Upper95 = High95CI)
#data.in<-left_join(data.in, names)
data.in$salt[data.in$location=="China"]<-4.83*2.54
length(unique(data.in$location))

setwd("../model")

#add scale-up data
inc<-read.csv("covfxn2.csv", stringsAsFactors = F)%>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)

bpcats<-c("<120", "120-129", "130-139", 
       "140-149", "150-159", "160-169", 
       "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)


#source("functions_review_4.R")
source("functions_review_6.R")

#ref<-fread("global_pop_0122.csv")
#check rates
b_rates[is.na(covid.mx), covid.mx:=0]
b_rates[covid.mx>=1, covid.mx:=0.9]

#rebalance TPs w/ covid such that they sum to less than 1
#especially @ old ages where covid deaths are high
b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[,check_sick := BG.mx+covid.mx+CF]

#first ensure that background mortality + covid <1
b_rates[check_well>1 | check_sick>1, covid.mx:=ifelse(1-BG.mx<covid.mx, 1-BG.mx, covid.mx)]
#then proportionally reduce rates by check_well
b_rates[check_well>1, covid.mx:= covid.mx - covid.mx*(check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, BG.mx   := BG.mx    - BG.mx*   (check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, IR      := IR       - IR*      (check_well-1)/(covid.mx+BG.mx+IR)]

b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[check_well>1]

#same process for check_sick
b_rates[check_sick>1, covid.mx:= covid.mx - covid.mx*(check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, BG.mx   := BG.mx    - BG.mx*   (check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, CF      := CF       - CF*      (check_sick-1)/(covid.mx+BG.mx+CF)]

b_rates[,check_sick := BG.mx+covid.mx+CF]
b_rates[check_sick>1]

#check that no BG.mx.all+covid>1
b_rates[covid.mx+BG.mx.all>1]

############
###fxn
repYear<-function(row){
  2017+floor((row-1)/224)
}

data.in<-data.table(data.in%>%select(-age)%>%rename(age=Age.group))
b_rates[, newcases:=0]
#not adjusting IR because we're essentially doing that with bau coverage
#b_rates[year>2019, IR:=IR*(0.997)^(year-2019)]
#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
  #################################################################################################
  base_rates<-b_rates[location==Country]#[, -c("year")]
  #################################################################################################
  #################################################################################################
  #intervention scenarios
  DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  
  bp_prob_salt<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_drug<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_both<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_base<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 0, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_bau<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, "baseline")
  
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
  
  ##add CF effects##
  #this is ugly code
  
  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="ihd",     CF:=CF*(1-0.24*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="istroke", CF:=CF*(1-0.36*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hstroke", CF:=CF*(1-0.76*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hhd",     CF:=CF*(1-0.20*aroc)]
  
  if(drugcov=="p75"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*p_change)]
  }
  
  if(drugcov=="p975"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*a_change)]
  }
  
  if(drugcov=="ideal"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*ideal)]
  }

  ##########################################################################################

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
  for(i in 1:41){
    
    b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
    b2[,age2:=age+1]
    
    #newcases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+BG.mx+covid.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    b2[sick2<0, sick2:=0]
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
    b2[dead2<0, dead2:=0]
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
    b2[pop2<0, pop2:=0] #prevent negatives
    
    #all dead envelope
    b2[,all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
    b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)+(pop2*covid.mx)] #UPDATE w/ covid data
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
  
  return(out.df)
  
  
}#as a fxn


#test that it doesn't crash before running for ~15 mins
#this takes ~15 seconds
test<-project.all("China",  "percent", 0.3, 2027, "p975")
p<-test%>%group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            sick=sum(sick))

ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()

ggplot(p, aes(x=year, y=sick, color=intervention))+
  geom_point()
######################################################################
#run in parallel
#need CPU w/ at at least 30 cores

######################################################################
library(foreach)
library(snow)
library(parallel)
library(iterators)
library(doParallel)

parallelCluster <- makeCluster(30, type = "SOCK",methods = FALSE,outfile="log.txt")

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    
  # set each cluster to single thread if worried about thrashing
  pacman::p_load(data.table, dplyr, tidyr)  
  # make sure we load the packages required on each cluster
  #setDTthreads(1)     
})

par_fxn <- function(is) {
   return(project.all(is,  "percent", 0.15, 2030, "p75"))
}    

# Run function for progress scenario
Sys.time()
progress <- foreach (is = countrylist, .combine ='rbind') %dopar% {
  par_fxn(is)
}
Sys.time()
#End timer
#~6 mins for 30 clusters

# Run function for aspirational scenario
par_fxn <- function(is) {
  return(project.all(is, "percent",  0.3,  2027, "p975"))
}    

Sys.time()
aspirational <- foreach (is = countrylist, .combine ='rbind') %dopar% {
  par_fxn(is)
}
Sys.time()


stopCluster(parallelCluster)

on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    stopCluster(parallelCluster) # package: `parallel`
  })
})


#gut check cumulative deaths and cases averted... abstract #s
progress%>%filter(intervention%in%c("Both", "b.a.u"))%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA = `b.a.u` - Both)%>%
  pull(DA)

progress%>%filter(intervention%in%c("Both", "b.a.u"))%>%
  group_by(intervention)%>%
  summarise(sick=sum(newcases))%>%
  spread(intervention, sick)%>%
  mutate(CA = `b.a.u` - Both)%>%
  pull(CA)

aspirational%>%filter(intervention%in%c("Both", "b.a.u"))%>%
  group_by(intervention)%>%
  summarise(dead=sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(DA = `b.a.u` - Both)%>%
  pull(DA)

aspirational%>%filter(intervention%in%c("Both", "b.a.u"))%>%
  group_by(intervention)%>%
  summarise(sick=sum(newcases))%>%
  spread(intervention, sick)%>%
  mutate(CA = `b.a.u` - Both)%>%
  pull(CA)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = c("plot", "parallelCluster", "all", "add50",
            "cfr", "df", "rep50", "names", 'test', 'p', 'wpp.adj'))
save.image(file = "model_output_newnorm.Rda")
#write.csv(everything, "progress.csv", row.names = F)
#write.csv(everything3, "aspirational.csv", row.names = F)
###################################################################################################
#
###################################################################################################
load("model_output_newnorm.Rda")
#compare population and all cause deaths
library(readxl)
malepop <- read_excel("../data_preprocessing/WPP2019_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx", 
                      sheet = "MEDIUM VARIANT", skip = 16, 
                      col_types = c("guess", "guess", "guess", "guess",
                                    "guess", "guess", "guess", "guess",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric"))%>%mutate(sex="Male")
femalepop <- read_excel("../data_preprocessing//WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx", 
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

unique(wpp.pop$location)

wpp.names<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_who, location_gbd)%>%rename(location = location_who)

wpp.names$location[wpp.names$location=="CÃ´te d'Ivoire"]<-"Côte d'Ivoire"

wpp.pop<-left_join(wpp.pop, wpp.names)%>%rename(loc = location, location = location_gbd)
wpp.pop$location[wpp.pop$loc=="WORLD"]<-"World"

wpp.pop<-wpp.pop%>%filter(location %in% c(countrylist, "World"))%>%
  select(-loc)

maledeaths <- read_excel("../data_preprocessing/WPP2019_MORT_F04_2_DEATHS_BY_AGE_MALE.xlsx", 
                      sheet = "MEDIUM VARIANT", skip = 16, 
                      col_types = c("guess", "guess", "guess", "guess",
                                    "guess", "guess", "guess", "guess",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric",
                                    "numeric", "numeric", "numeric", "numeric"))%>%
                      mutate(sex="Male")%>%rename("95-99" = `95+`)
femaledeaths <- read_excel("../data_preprocessing/WPP2019_MORT_F04_3_DEATHS_BY_AGE_FEMALE.xlsx", 
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

#model<-out.df%>%filter(intervention == "b.a.u")
model<-aspirational%>%filter(intervention == "b.a.u")
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
                 wpp.pop%>%filter(year<=2055, location=="World", substring(age.group,1,1)>=2, 
                                  age.group!="95-99", age.group!="5-9")%>%
                   group_by(year, source, age.group)%>%summarise(pop=sum(pop)*1000))

test<-plot2%>%spread(source, pop)

ggplot(test, aes(x=year))+
  geom_line(aes(y=model/1e6))+
  geom_ribbon(aes(ymin = WPP*0.95/1e6, ymax=WPP*1.05/1e6), fill="blue", alpha=0.3)+
  facet_wrap(~age.group)+
  ylab("Population (millions)")+
  xlab("Year")

ggsave("../output/world_pop.png", height=8, width=8)

which(countrylist %in% unique(wpp.pop$location))

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

ggsave("../output/China_pop.png", height=8, width=8)

regions<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(wb2021, location_gbd)%>%rename(location = location_gbd)

model.pop<-left_join(model.pop, regions)
wpp.pop<-left_join(wpp.pop, regions)

#write.csv(model.pop, "../web_appendix/shiny/pop.csv", row.names = F)
#write.csv(wpp.pop, "../web_appendix/shiny/wpp.csv", row.names = F)
#write.csv(regions, "../web_appendix/shiny/regions.csv", row.names = F)

#deaths#
#intervention_out<-read.csv("~/RTSL/model/intervention_out_ref_2021.csv", stringsAsFactors = F)
outDT<-read.csv("forecasts2019.csv", stringsAsFactors = F)
gbdfs<-outDT%>%group_by(year, cause, intervention, location)%>%summarise(Deaths=sum(Deaths))%>%
  rename(Scenario = intervention)
gbdfs$Scenario[gbdfs$Scenario=="gbd 2019"]<-"GBD 2019 data"
gbdfs$Scenario[gbdfs$Scenario=="GBD 2019, Reference"]<-"GBD 'Reference' projection"
gbdfs$Scenario[gbdfs$Scenario=="GBD 2019, Worse"]<-"GBD 'Worse' projection"

plot4<-bind_rows(gbdfs, progress%>%filter(intervention =="b.a.u", location %in% countrylist)%>%
                   group_by(cause, year, location)%>%summarise(Deaths = sum(dead))%>%
                   mutate(Scenario = "Model 'Business as usual' projection"))



plot4$cause[plot4$cause=="hhd"]<-unique(plot4$cause)[1]
plot4$cause[plot4$cause=="hstroke"]<-unique(plot4$cause)[2]
plot4$cause[plot4$cause=="ihd"]<-unique(plot4$cause)[3]
plot4$cause[plot4$cause=="istroke"]<-unique(plot4$cause)[4]

plot4<-left_join(plot4, regions)
#write.csv(plot4, "../web_appendix/shiny/total_deaths.csv", row.names = F)
plot4<-plot4%>%group_by(year, cause, Scenario)%>%summarise(Deaths = sum(Deaths))

ggplot(plot4, aes(x=year, y=Deaths/1e6, color=Scenario))+
  geom_line(size=1)+
  facet_wrap(~cause)+
  ylab("Deaths (millions)")+
  xlab("Year")

ggsave("../output/gbdforesight.png", height = 8, width = 8)

rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
##############################################

dt1<-fread("gbd1.csv")
dt2<-fread("gbd2.csv")
dt3<-fread("gbd3.csv")
dt4<-fread("gbd4.csv")
dt5<-fread("gbd5.csv")
dt6<-fread("gbd6.csv")
dt7<-fread("gbd7.csv")
dt8<-fread("gbd8.csv")
dt9<-fread("gbd9.csv")
dt10<-fread("gbd10.csv")

dt<-rbindlist(list(dt1,dt2,dt3,dt4,dt5,dt6,dt7,dt8,dt9,dt10))
#dt<-fread("global95to19.csv")
dt[, upper:=NULL]
dt[, lower:=NULL]

## update GBD names ##
iso3<-fread("Country_groupings_extended.csv")
iso3<-iso3[, c("gbd2019", "location_gbd")]
setnames(iso3, "gbd2019", "location")
dt<-merge(dt, iso3, by="location")
dt[,location:=NULL]
setnames(dt, "location_gbd", "location")


#as a function#

get.new.rates<-function(dt, year1, year2){

prevyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year1)%>%
                select(-c(measure, metric, year))%>%
                  rename(prev14 = val))

prevrtyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt14 = val))

deathyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(death14 = val))

deathrtyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt14 = val))

prevyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prev19 = val))

prevrtyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt19 = val))

deathyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(death19 = val))

deathrtyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt19 = val))

mymerge<-function(x,y){merge.data.table(x,y, by=c("location", "age", "sex", "cause"))}

dt14<-Reduce(mymerge, list(prevyear1,prevrtyear1,deathyear1,deathrtyear1))
dt19<-Reduce(mymerge, list(prevyear2,prevrtyear2,deathyear2,deathrtyear2))

dt14[, pop14:=death14/deathrt14*100000]
dt19[, pop19:=death19/deathrt19*100000]

age14<-function(x){
  if(x=="20 to 24"){return<-1}
  if(x=="25 to 29"){return<-2}
  if(x=="30 to 34"){return<-3}
  if(x=="35 to 39"){return<-4}
  if(x=="40 to 44"){return<-5}
  if(x=="45 to 49"){return<-6}
  if(x=="50 to 54"){return<-7}
  if(x=="55 to 59"){return<-8}
  if(x=="60 to 64"){return<-9}
  if(x=="65 to 69"){return<-10}
  if(x=="70 to 74"){return<-11}
  if(x=="75 to 79"){return<-12}
  if(x=="80 to 84"){return<-13}
  if(x=="85 to 89"){return<-14}
  if(x=="90 to 94"){return<-15}
  if(x=="95 plus") {return<-16}
  return
  }

dt14[, age2:=sapply(age, age14)]

age19<-function(x){
  if(x=="20 to 24"){return<-0}
  if(x=="25 to 29"){return<-1}
  if(x=="30 to 34"){return<-2}
  if(x=="35 to 39"){return<-3}
  if(x=="40 to 44"){return<-4}
  if(x=="45 to 49"){return<-5}
  if(x=="50 to 54"){return<-6}
  if(x=="55 to 59"){return<-7}
  if(x=="60 to 64"){return<-8}
  if(x=="65 to 69"){return<-9}
  if(x=="70 to 74"){return<-10}
  if(x=="75 to 79"){return<-11}
  if(x=="80 to 84"){return<-12}
  if(x=="85 to 89"){return<-13}
  if(x=="90 to 94"){return<-14}
  if(x=="95 plus") {return<-15}
  return
}

dt19[, age2:=sapply(age, age19)]
dt19[, age:=NULL]

dt<-merge(dt14, dt19, by=c("age2", "location", "sex", "cause"))
allcause<-dt[cause=="All causes"]
setnames(allcause, c("death14", "deathrt14", "death19", "deathrt19"),
         c("alldeath14", "alldeathrt14", "alldeath19", "alldeathrt19"))
allcause[,c("cause", "prev14", "prevrt14", "prev19", "prevrt19",
            "pop14", "pop19"):=NULL]

dt<-merge(dt, allcause, by=c("age2", "sex", "location", "age"))
dt<-dt[cause!="All causes"]

dt[, othermx14:=alldeath14-death14]
dt[, othermx19:=alldeath19-death19]
dt[, othermxrt14:=alldeathrt14 - deathrt14]
dt[, othermxrt19:=alldeathrt19 - deathrt19]

dt[, well14:=pop14-prev14-alldeath14]
dt[,well19:=pop19-prev19-alldeath19]

dt[, wellAARC:=log((well19/pop19)/(well14/pop14))/5]
dt[, sickAARC:=log(prevrt19/prevrt14)/5]
dt[, deadAARC:=log(deathrt19/deathrt14)/5]
dt[, deadotherAARC:=log(othermxrt19/othermxrt14)/5]

dt[sickAARC<0, sickAARC:=0]
dt[sickAARC>1, sickAARC:=0.99]
dt[deadAARC<0, deadAARC:=0]
dt[deadAARC>1, deadAARC:=0.99]
dt[deadotherAARC<0, deadotherAARC:=0]
dt[deadotherAARC>1, deadotherAARC:=0.99]


dt[,age2:=1]
reprow<-function(row){
  floor((row-1)/24480)
}

DT<-dt[rep(seq(1,nrow(dt)), 5)][, age2:=age2+reprow(.I)]

DT[age2==1, Well:=well14]
DT[age2==1, Sick:=prev14]
DT[age2==1, Dead:=death14]
DT[age2==1, DeadOther:=othermx14]

for(i in 2:5){
DT2<-DT[age2<=i &age2>=i-1]
DT2[, Well2:=shift(Well)*(1+wellAARC), by=.(age, sex, location, cause)]
DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Well2")]
DT[age2==i, Well:=DT2[,Well2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Sick2:=shift(Sick)*(1+sickAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Sick2")]
  DT[age2==i, Sick:=DT2[,Sick2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Dead2:=shift(Dead)*(1+deadAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Dead2")]
  DT[age2==i, Dead:=DT2[,Dead2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, DeadOther2:=shift(DeadOther)*(1+deadotherAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "DeadOther2")]
  DT[age2==i, DeadOther:=DT2[,DeadOther2]]
}

DT[, IR:=(Sick-(shift(Sick)-Dead))/shift(Well),  by=.(age, sex, location, cause)]
DT[, CF:=Dead/shift(Sick),  by=.(age, sex, location, cause)]

DT[ , avgIR:=mean(na.omit(IR)), by=.(age, sex, location, cause)]
DT[ , avgCF:=mean(na.omit(CF)), by=.(age, sex, location, cause)]
DT[ , midptage:=as.numeric(substr(age,1,2))+2]

DT_final<-unique(DT[,c("midptage", "age", "sex", "location", "cause", "avgIR", "avgCF")])
DT_final[, year:=year2]

DT_final[avgIR<0 | is.na(avgIR), avgIR:=0]
DT_final[avgCF>1, avgCF:=0.99]
DT_final[avgIR>1, avgIR:=0.99]
}
#end of function

newrates<-get.new.rates(dt, 1995,2000)

for(i in 1:19){
  DT_final<-  get.new.rates(dt, 1995+i, 2000+i)
  newrates<-rbindlist(list(DT_final, newrates), use.names = T)
}

DT_final<-newrates #store for debugging
which(is.na(DT_final))
#write.csv(DT_final, "global_rates.csv")

#change format to match Markov model code#
DT_final$cause[DT_final$cause=="Ischemic heart disease"]<-"ihd"
DT_final$cause[DT_final$cause=="Hypertensive heart disease"]<-"hhd"
DT_final$cause[DT_final$cause=="Ischemic stroke"]<-"istroke"
DT_final$cause[DT_final$cause=="Intracerebral hemorrhage"]<-"hstroke"

over95<-DT_final[age=="90 to 94"]
over95[, age:="95 plus"]
over95[, midptage:=97]

new<-rbindlist(list(over95, DT_final))
new<-new[order(location, sex, midptage)]

setnames(new, c("avgIR", "avgCF"), c("IR", "CF"))

get.data <- function(sx, cse, var, dfin, yr, country){
  x        <- c(seq(22,92,5), 95)
  y        <- dfin %>% filter(sex==sx & cause==cse) %>% pull(var)
  d        <- approx(x,y, xout=20:95, rule=2, method="linear")
  df       <- data.table(age = d$x, dname = d$y, cause = cse, sex = sx, location = country, year = yr)
  setnames(df, "dname", var)
  df
}

get.single.age.rates<-function(data, yr, country){
  
data<-data[year==yr & location==country]
  
d<-get.data("Female", "ihd", "IR", data, yr, country)
d2<-get.data("Male", "ihd", "IR", data, yr, country)
d3<-get.data("Female", "hhd", "IR", data, yr, country)
d4<-get.data("Male", "hhd", "IR", data, yr, country)
d5<-get.data("Female", "istroke", "IR", data, yr, country)
d6<-get.data("Male", "istroke", "IR", data, yr, country)
d7<-get.data("Female", "hstroke", "IR", data, yr, country)
d8<-get.data("Male", "hstroke", "IR", data, yr, country)

IRs<-rbindlist(list(d,d2,d3,d4,d5,d6,d7,d8))

d<-get.data("Female", "ihd", "CF", data, yr, country)
d2<-get.data("Male", "ihd", "CF", data, yr, country)
d3<-get.data("Female", "hhd", "CF", data, yr, country)
d4<-get.data("Male", "hhd", "CF", data, yr, country)
d5<-get.data("Female", "istroke", "CF", data, yr, country)
d6<-get.data("Male", "istroke", "CF", data, yr, country)
d7<-get.data("Female", "hstroke", "CF", data, yr, country)
d8<-get.data("Male", "hstroke", "CF", data, yr, country)

CFs<-rbindlist(list(d,d2,d3,d4,d5,d6,d7,d8))

newrates2<-merge(IRs, CFs, by=c("age", "sex", "cause", "location", "year"))
newrates2
}

newrates<-get.single.age.rates(new, 2000, "Afghanistan")
newrates<-rbindlist(list(newrates,
                         get.single.age.rates(new, 2001, "Afghanistan"), get.single.age.rates(new, 2002, "Afghanistan"),
                         get.single.age.rates(new, 2003, "Afghanistan"), get.single.age.rates(new, 2004, "Afghanistan"),
                         get.single.age.rates(new, 2005, "Afghanistan"),
                         get.single.age.rates(new, 2006, "Afghanistan"), get.single.age.rates(new, 2007, "Afghanistan"),
                         get.single.age.rates(new, 2008, "Afghanistan"), get.single.age.rates(new, 2009, "Afghanistan"),
                         get.single.age.rates(new, 2010, "Afghanistan"), get.single.age.rates(new, 2011, "Afghanistan"),
                         get.single.age.rates(new, 2012, "Afghanistan"), get.single.age.rates(new, 2013, "Afghanistan"),
                         get.single.age.rates(new, 2014, "Afghanistan"), get.single.age.rates(new, 2015, "Afghanistan"),
                         get.single.age.rates(new, 2016, "Afghanistan"), get.single.age.rates(new, 2017, "Afghanistan"),
                         get.single.age.rates(new, 2018, "Afghanistan"), get.single.age.rates(new, 2019, "Afghanistan")))

countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global")%>%pull(location)

for (i in 2:195) {
  dat <- get.single.age.rates(new, 2000, countrylist[i])
  dat<-rbindlist(list(dat,
                      get.single.age.rates(new, 2001, countrylist[i]), get.single.age.rates(new, 2002, countrylist[i]),
                      get.single.age.rates(new, 2003, countrylist[i]), get.single.age.rates(new, 2004, countrylist[i]),
                      get.single.age.rates(new, 2005, countrylist[i]),
                      get.single.age.rates(new, 2006, countrylist[i]), get.single.age.rates(new, 2007, countrylist[i]),
                      get.single.age.rates(new, 2008, countrylist[i]), get.single.age.rates(new, 2009, countrylist[i]),
                      get.single.age.rates(new, 2010, countrylist[i]), get.single.age.rates(new, 2011, countrylist[i]),
                      get.single.age.rates(new, 2012, countrylist[i]), get.single.age.rates(new, 2013, countrylist[i]),
                      get.single.age.rates(new, 2014, countrylist[i]), get.single.age.rates(new, 2015, countrylist[i]),
                      get.single.age.rates(new, 2016, countrylist[i]), get.single.age.rates(new, 2017, countrylist[i]),
                      get.single.age.rates(new, 2018, countrylist[i]), get.single.age.rates(new, 2019, countrylist[i])))
                      
  newrates<-rbindlist(list(dat,newrates))
}

ggplot(newrates%>%filter(location=="Afghanistan", sex=="Male", cause=="hstroke", year==2000), aes(x=age, y=IR))+
  geom_point()

############# Combine merge with other rates #############
other<-fread("baseline_rates_noIR2019.csv")

dataout<-merge(other, newrates, by=c("age", "sex", "location", "year", "cause"))
any(is.na(dataout))

write.csv(dataout, "baseline_rates_new_new2019.csv", row.names = F)

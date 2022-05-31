#################################################################################################

rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bgmx<-fread("bgmx.csv")

iso3<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%select(gbd2019, location_gbd)%>%
  rename(location = gbd2019)

bgmx<-left_join(bgmx, iso3, by="location")%>%select(-c(location))%>%rename(location = location_gbd)%>%select(-c(upper, lower, measure, metric))

#inside function
get.bg.df<-function(Country){
  
data<-bgmx%>%filter(location==Country)%>%spread(cause, val)
data$bg<-data$`All causes`- data$`Ischemic heart disease` - data$`Hypertensive heart disease`-data$`Ischemic stroke`-data$`Intracerebral hemorrhage`
 
data<-data%>%filter(year==2010 | year==2017)%>%select(c(age, sex, location, bg, year))
data<-data%>%spread(year, bg)

data$val<-((data$`2017`- data$`2010`)/data$`2010`)/7
data$test<-data$`2010`*(1+data$val)^20

 
  get.data <- function(sx, var, dfin){
    x        <- c(seq(22,92,5), 95)
    y        <- dfin %>% filter(sex==sx) %>% pull(val)
    d        <- approx(x,y, xout=20:95, rule=2, method="linear")
    df       <- data.table(age = d$x, dname = d$y, sex = sx, location = Country)
    setnames(df, "dname", var)
    df
  }
  
  jvars          <- c("age","sex","location","year")
  
  d<-get.data("Male", "diff", data)
  d1<-get.data("Female", "diff", data)
  
  
  data<-rbind(d,d1)
  
}

#test
test<-get.bg.df("Afghanistan")

ggplot(test, aes(x=age, y=diff))+geom_point()+facet_wrap(~sex)

#run for all countries
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


for (i in 2:182) {
  # ... make some data
  dat <- get.bg.df(countrylist[i])
  test<-bind_rows(dat,test)
  #dat$i <- i  # maybe you want to keep track of which iteration produced it?
  #datalist[[i]] <- dat # add it to your list
}

write.csv(test, "bg_diffs.csv", row.names = F)



#################################################################################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

gbd_data<-fread("cf_data.csv")

iso3<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%select(gbd2019, location_gbd)%>%
  rename(location = gbd2019)

gbd_data<-left_join(gbd_data, iso3, by="location")%>%select(-c(location))%>%rename(location = location_gbd)

get.cf.df<-function(Country){
  
  cf<-gbd_data%>%filter(location==Country)    
  
  deaths<-cf%>%filter(measure=="Deaths")
  cases<-cf%>%filter(measure=="Prevalence")
  
  names(deaths)[7]<-"deaths"
  names(cases)[7]<-"cases"
  
  deaths<-deaths%>%select(-c(measure, upper, lower))
  cases<-cases%>%select(-c(measure, upper, lower))
  
  cf<-left_join(deaths, cases, by=c("location", "age", "sex", "cause", "year", "metric"))
  
  cf$casefatality<-cf$deaths/cf$cases
  cf$casefatality[cf$casefatality>0.99]<-0.99
  cf2<-cf%>%select(-c("deaths", "cases"))%>%spread(year, casefatality)
  
  cf2$val<-((cf2$`2017`- cf2$`2010`)/cf2$`2010`)/7
  cf2$test<-cf2$`2010`*(1+cf2$val)^20
  
  
  get.data <- function(sx, cse, var, dfin){
    x        <- c(seq(22,92,5), 95)
    y        <- dfin %>% filter(sex==sx & cause==cse) %>% pull(val)
    d        <- approx(x,y, xout=20:95, rule=2, method="linear")
    df       <- data.table(age = d$x, dname = d$y, sex = sx, location = Country, year = 2017)
    setnames(df, "dname", var)
    df
  }
  
  jvars          <- c("age","sex","location","year")
  
  d<-get.data("Male", "Ischemic heart disease", "diff", cf2)
  d$cause<-"Ischemic heart disease"
  d1<-get.data("Female", "Ischemic heart disease", "diff", cf2)
  d1$cause<-"Ischemic heart disease"
  d2<-get.data("Male", "Ischemic stroke", "diff", cf2)
  d2$cause<-"Ischemic stroke"
  d3<-get.data("Female", "Ischemic stroke", "diff", cf2)
  d3$cause<-"Ischemic stroke"
  d4<-get.data("Male", "Hypertensive heart disease", "diff", cf2)
  d4$cause<-"Hypertensive heart disease"
  d5<-get.data("Female", "Hypertensive heart disease","diff", cf2)
  d5$cause<-"Hypertensive heart disease"
  d6<-get.data("Male", "Intracerebral hemorrhage", "diff", cf2)
  d6$cause<-"Intracerebral hemorrhage"
  d7<-get.data("Female", "Intracerebral hemorrhage", "diff", cf2)
  d7$cause<-"Intracerebral hemorrhage"
  
  data<-rbind(d,d1,d2,d3,d4,d5,d6,d7)
  
}

test<-get.cf.df("Afghanistan")

ggplot(test, aes(x=age, y=diff, color=cause))+geom_point()+facet_wrap(~sex)


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



for (i in 2:182) {
  # ... make some data
  dat <- get.cf.df(countrylist[i])
  test<-bind_rows(dat,test)
  #dat$i <- i  # maybe you want to keep track of which iteration produced it?
  #datalist[[i]] <- dat # add it to your list
}

write.csv(test, "cf_diffs.csv", row.names = F)

###########################################
#Code to get gamma distribution attributes for 
#coutnry-age-sex-specific SBP
###########################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, readxl)   
############################################

## BP distributions for every country ##
SBP <-bind_rows(read.csv("NCD-RisC_Lancet_2017_Men_Agespecific_Mean_SBP_by_Country.csv",
                         stringsAsFactors = F)%>%mutate(sex="Male"),
                read.csv("NCD-RisC_Lancet_2017_Women_Agespecific_Mean_SBP_by_Country.csv",
                         stringsAsFactors = F)%>%mutate(sex="Female"))%>%
  filter(Year == max(Year), Age.group!="18-19")%>%
  rename(Mean = Mean.systolic.blood.pressure..mmHg.,
         Lower95 = Lower.95..uncertainty.interval..mmHg.,
         Upper95 = Upper.95..uncertainty.interval..mmHg.)

#Updating to GBD names
locs<-read.csv("../Country_groupings_extended.csv", stringsAsFactors = F)

SBP<-left_join(locs%>%
                 select(location_ncdrisc, iso3, gbd2019, location_gbd)%>%
                 rename(Country = location_ncdrisc), 
               SBP,
               by="Country")

#Missing any GBD 2019 locations?
SBP%>%filter(is.na(Country), Age.group=="18-19", sex=="Female")%>%pull(gbd2019)
#no
any(is.na(SBP))
SBP<-na.omit(SBP)

#add proportion of population with raised blood pressure
raisedBP<-bind_rows(read.csv("fem_raisedBP.csv", stringsAsFactors = F)%>%mutate(sex="Female"),
                    read.csv("male_raisedBP.csv", stringsAsFactors = F)%>%mutate(sex="Male"))%>%
  filter(Year==max(Year), Age.group!="18-19")%>%
  rename(raisedBP = Raised.blood.pressure.prevalence)%>%
  select(Country, raisedBP, Age.group, sex)


SBP<-left_join(SBP, raisedBP, by=c("Country", "sex", "Age.group"))
SBP<-data.frame(SBP)

#Assuming gamma distribution, get standard deviation
prior1<-0
prior2<-50
n<-10000
set.seed(123)
guesses<-runif(n, min=prior1, max=prior2)

get.stdev<-function(mean,htn){
  data.frame(error=sqrt((htn-pgamma(q=140, shape=(mean/guesses)^2, scale=(guesses^2)/mean, lower.tail = F))^2),
             stdev = guesses)%>%filter(error == min(error))%>%pull(stdev)
}

#takes ~ 1 min
#run and check error
SBP<-bind_cols(SBP, stdev=mapply(get.stdev, SBP$Mean, SBP$raisedBP))%>%
  mutate(htn2 = pgamma(q=140, shape=(Mean/stdev)^2, scale=(stdev^2)/Mean, lower.tail = F),
         error = 100*sqrt(((raisedBP-htn2))^2))

#a few estimates seem to be quite off, probably due to skewed data
#but in most of these cases our estimates are under estimating the proportion w/ hypertension
#giving us a more conservative estimate on impact
hist(SBP%>%mutate(diff = 100*(htn2-raisedBP))%>%pull(diff))

##compare to nhanes##
nhanes<-read.csv("bp_data3.csv")%>%select(age, stdev, sex)%>%
  rename(Age.group = age)%>%
  mutate(Source = "NHANES")%>%
  unique()

#relative differences for ages 55+
diffs<-nhanes%>%
  mutate(age = as.numeric(substr(Age.group,1,2)))%>%
  arrange(sex,age)%>%group_by(sex)%>%
  mutate(rdiff = shift(stdev, type='lead')/stdev,
         rdiff = ifelse(is.na(rdiff),1,rdiff))


avg<-SBP%>%group_by(Age.group, sex)%>%
                  summarise(stdev=mean(stdev))%>%
  mutate(Source="derived", Country="Simple average")

plot<-bind_rows(avg, SBP%>%mutate(Source = "derived")%>%
                  select(Age.group, sex, Country, Source, stdev))

nhanes<-left_join(plot%>%select(Age.group, sex, Country), nhanes)

plot<-bind_rows(plot, nhanes)%>%
  filter(Country%in%c("Simple average", "Canada", "United States of America", 
                      "China", "India", "Brazil", "Ethiopia"))

plot<-data.frame(plot)%>%mutate(age = as.numeric(substr(Age.group, 1,2)))

ggplot(plot, aes(x=age, y=stdev, color=Source))+
  geom_point()+
  facet_grid(sex~Country)+
  ylab("Standard deviation")+
  xlab("Age")+
  theme_bw()

ggsave("BP_stdevs_gamma.png", height = 8, width=12)

#################try normal distribution
prior1<-0
prior2<-50
n<-10000
set.seed(123)
guesses<-runif(n, min=prior1, max=prior2)

norm.stdev<-function(mean,htn){
  data.frame(error=sqrt((htn-(1-pnorm(140,mean,guesses)))^2),
             stdev = guesses)%>%filter(error == min(error))%>%pull(stdev)
}

#takes ~ 1 min
#run and check error
SBP<-bind_cols(SBP, stdev_norm=mapply(norm.stdev, SBP$Mean, SBP$raisedBP))%>%
  mutate(htn3 = 1-pnorm(140, Mean, stdev_norm),
         error3 = 100*sqrt(((raisedBP-htn3))^2))

hist(SBP%>%mutate(diff = 100*(htn3-raisedBP))%>%pull(diff))

#adjust for ages 55+
SBP<-left_join(SBP, diffs%>%select(Age.group, sex, rdiff))%>%
  mutate(age = as.numeric(substr(Age.group,1,2)))

for(i in c(55,60,65,70,75,80,85)){
  SBP<-SBP%>%group_by(location_gbd, sex)%>%arrange(age)%>%
  mutate(stdev_norm = ifelse(age==i, 
                             shift(stdev_norm, type='lag')*rdiff,
                             stdev_norm))
}
                 
SBP<-SBP%>%select(-rdiff, -age) 

#compare
norm.avg<-SBP%>%group_by(Age.group, sex)%>%
  summarise(stdev=mean(stdev_norm))%>%
  mutate(Source="derived_normal", Country="Simple average")

norm<-bind_rows(norm.avg, SBP%>%mutate(Source = "derived_normal")%>%
                  select(Age.group, sex, Country, Source, stdev_norm)%>%
                  rename(stdev = stdev_norm))%>%
                  mutate(age = as.numeric(substr(Age.group,1,2)))

plot<-bind_rows(plot, norm)%>%
  filter(Country%in%c("Simple average", "Canada", "United States of America", 
                       "China", "India", "Brazil", "Ethiopia"))

ggplot(plot%>%mutate(Source = ifelse(Source=="derived", "derived_gamma", Source))%>%
         filter(Country%in%c("Simple average", "United States of America", "Canada",
                             "India", "China", "Ethiopia"),
                Source!="derived_gamma"), 
       aes(x=age, y=stdev, color=Source))+
  geom_point()+
  geom_line()+
  facet_grid(sex~Country)+
  ylab("Standard deviation")+
  xlab("Age")+
  theme_bw()

ggsave("BP_stdevs_norm.png", height = 8, width=12)

##### try different calc for normal distribution, imposing lower bound

#run and check error
SBP<-SBP%>%
  mutate(age = as.numeric(substr(Age.group,1,2)),
         low5 = ifelse(sex=="Female" & age<40,  96,
                ifelse(sex=="Male"   & age<40, 100,
                ifelse(sex=="Female" & age>=40 & age<60, 100,
                ifelse(sex=="Male"   & age>=40 & age<60, 102,
                ifelse(sex=="Female" & age>=60, 110,
                ifelse(sex=="Male"   & age>=60 & age<80, 113, 118)))))),
         stdev_norm2 = (low5-Mean)/qnorm(0.05),
         htn4 = 1-pnorm(140, Mean, stdev_norm2),
         error4 = 100*sqrt(((raisedBP-htn4))^2))

hist(SBP%>%mutate(diff = 100*(htn4-raisedBP))%>%pull(diff))

ggplot(SBP, aes(x = raisedBP, y=htn4))+
  geom_point()+
  facet_wrap(~sex)+ 
  geom_abline(intercept=0, slope=1, size=1, color='red')+
  xlab("NCD-Risc")+
  ylab("Modeled")+
  ggtitle("Prevalence of raised BP")

ggsave("htn_compare.png", height = 10, width=12)

###############
#calculate ICC
###############
library(irr)
test<-SBP%>%ungroup()%>%select(htn4, raisedBP)
icc(test, model = 'twoway', type = 'agreement', unit='single')

###
#more plots
###

ggplot(SBP%>%mutate(Age = ifelse(age<50, "Under 50", "50 plus")), aes(x = raisedBP, y=htn4))+
  geom_point()+
  facet_wrap(Age~sex)+ 
  geom_abline(intercept=0, slope=1, size=1, color='red')+
  xlab("NCD-Risc")+
  ylab("Modeled")+
  ggtitle("Prevalence of raised BP")

ggsave("htn_compare_age.png", height = 10, width=12)

names<-read.csv("../Country_groupings_extended.csv")%>%
  select(wb2021, location_ncdrisc)%>%
  rename(Country = location_ncdrisc)

ggplot(left_join(SBP,names), aes(x = raisedBP, y=htn4))+
  geom_point()+
  facet_wrap(~wb2021)+ 
  geom_abline(intercept=0, slope=1, size=1, color='red')+
  xlab("NCD-Risc")+
  ylab("Modeled")+
  ggtitle("Prevalence of raised BP")

ggsave("htn_compare_region.png", height = 10, width=12)

#compare
norm2.avg<-SBP%>%group_by(Age.group, sex)%>%
  summarise(stdev=mean(stdev_norm2))%>%
  mutate(Source="new_normal", Country="Simple average")

norm2<-bind_rows(norm2.avg, SBP%>%mutate(Source = "new_normal")%>%
                  select(Age.group, sex, Country, Source, stdev_norm2)%>%
                  rename(stdev = stdev_norm2))%>%
  mutate(age = as.numeric(substr(Age.group,1,2)))

plot2<-bind_rows(plot, norm2)%>%
  filter(Country%in%c("Simple average", "Canada", "United States of America", 
                      "China", "India", "Brazil", "Ethiopia", "Nigeria", "South Africa",
                      "Mexico", "Indonesia", "Germany"))

ggplot(plot2%>%filter(Source=="new_normal"), 
       aes(x=age, y=stdev, color=Source))+
  geom_line()+
  geom_point()+
  geom_line(data = plot2%>%filter(Source=="NHANES")%>%select(-Country), 
            aes(x=age, y=stdev, color=Source))+
  facet_grid(sex~Country)+
  ylab("Standard deviation")+
  xlab("Age")+
  theme_bw()+
  ylim(0,30)

ggsave("BP_stdevs_newnormal.png", height = 6, width=12)

################
##take 2nd normal cal
###############
SBP<-SBP%>%
  select(-error, -error3, -htn2, - htn3, -stdev, -error4, -htn4, stdev_norm)%>%
  rename(stdev = stdev_norm2)

#Add prevalence of diabetes for treatment protocol (control to <130 vs.<140)
#Data from GBD 2019
diabetes <- read.csv("diabetes2.csv", stringsAsFactors=FALSE)%>%
  rename(Age.group = age,
         diabetes = val,
         location_gbd = location)%>%
  mutate(Age.group = ifelse(Age.group=="85 to 89", "85plus", gsub(" to ", "-", Age.group)))%>%
  select(Age.group, sex, location_gbd, diabetes)%>%
  filter(Age.group!="90-94", Age.group!="95 plus")

unique(diabetes%>%arrange(location_gbd)%>%pull(location_gbd))
SBP<-left_join(SBP, diabetes)

####adding baseline salt and HTN coverage
salt<-read.csv("Adults (age 25+ years)_ Estimated per capita sodium intake_4-3-2021 11.50.csv", 
               stringsAsFactors = F)%>%
  rename(iso3=AreaID, salt=DataValue)%>%
  select(c(iso3, salt))
salt$salt<-salt$salt*2.54 #converting sodium to salt

htncov<-read.csv("HTN_cov.csv", stringsAsFactors = F)%>%
  select(Location, eff_HTN_cov)%>%
  rename(location_gbd = Location, htncov = eff_HTN_cov)

SBP<-left_join(SBP, salt)

SBP<-left_join(SBP, htncov)
nrow(is.na(SBP$htncov))

#Assumption that South Sudan has same BP distribution as Sudan
SBP<-bind_rows(SBP,
               SBP%>%filter(Country=="Sudan")%>%
                 mutate(Country="South Sudan",
                        gbd2019 = "South Sudan",
                        iso3 = "SSD",
                        location_gbd = "South Sudan"))

SBP%>%filter(Country == "South Sudan")
unique(SBP$location_gbd)
##################################################################################
#considering different control estimates
ncdrisc <- read.csv("NCD-RisC_Model81_Hypertension_results_2019.csv", stringsAsFactors=FALSE)
unique(ncdrisc$Year)
ncdrisc<-ncdrisc[,c(1,2,5,6)]
ncdrisc<-ncdrisc%>%spread(Variable, Prevalence.or.proportion)

gdp <- read_excel("gdp.xlsx", sheet=2)
gdp<-gdp[,c(1,3)]
names(gdp)[1]<-"Country"

steps <- read.csv("htn_data.csv", stringsAsFactors=FALSE)

plot<-left_join(gdp, ncdrisc, by="Country")
names(steps)[2]<-"Country"
names(plot)[5]<-"Controlled"
names(plot)[6]<-"Aware"
names(plot)[7]<-"Treated"
plot$data<-"NCDRisC"
plot2<-left_join(gdp, steps, by="Country")
plot2$data<-"STEPS"
plot3<-full_join(plot, plot2)

ggplot(plot3, aes(x=log(pcgdp), y=Controlled, color=data))+
  geom_point()
ggplot(plot3, aes(x=log(pcgdp), y=Aware, color=data))+
  geom_point()

###################################################
risc <- read.csv("NCD-RisC_Model81_Hypertension_results_2019.csv", stringsAsFactors=FALSE)
plot4<-left_join(gdp, risc, by="Country")

unique(plot4$Variable)
ggplot(plot4%>%filter(Variable=="Proportion with untreated stage 2 hypertension among hypertension" | 
                        Variable=="Proportion controlled among hypertenion"),
       aes(x=pcgdp, y=Prevalence.or.proportion, color=Variable))+
  geom_point()+ 
  theme(legend.position="bottom")


##update bp data for epi model
risc <- read.csv("NCD-RisC_Model81_Hypertension_results_2019.csv", stringsAsFactors=FALSE)

control<-risc%>%filter(Variable=="Proportion controlled among hypertenion")
control$Sex[control$Sex=="Women"]<-"Female"
control$Sex[control$Sex=="Men"]<-"Male"

control<-control[,c(1,2,6)]
names(control)[1]<-"location_ncdrisc"
names(control)[2]<-"sex"
names(control)[3]<-"htncov2"

control<-control%>%rename(Country = location_ncdrisc)

merge<-left_join(SBP, control, by=c("Country", "sex"))

unique(merge$location_gbd)
nrow(is.na(merge$htncov2))

#################################################################################
#end
write.csv(merge, "bp_data6.csv", row.names = F)
write.csv(merge, "../bp_data6.csv", row.names = F)
##################################################################################


#################################################################################
#scratch pad
################################################################################

#Hard coded example
#sovlve numberically for stdev knowing mean and P(x>140):
#Scale=stdev^2/Mean
#Shape=(Mean/stdev)^2

#example: mean = 119.5522 and p(x>140) = 0.11149048

#minimize error
#10,000 tries
# priors [0,20]
set.seed(123)
error<-vector(mode="numeric", length=10000)
guesses<-runif(10000, min=0, max=20)
k<-1

for (i in guesses){
error[k] = sqrt((0.11149048 - pgamma(q=140, shape=(119.5522/i)^2, scale=(i^2)/119.5522, lower.tail = F))^2)
k<-k+1
}

out<-data.frame(stdev=guesses, er=error)%>%filter(er==min(er))%>%pull(stdev)

#check ~=0.11149048
pgamma(q=140, shape=(119.5522/out)^2, scale=(out^2)/119.5522, lower.tail = F)
#percent error
(0.11149048-pgamma(q=140, shape=(119.5522/out)^2, scale=(out^2)/119.5522, lower.tail = F))*100

##################################
##as a function##
#################################
prior1<-0
prior2<-25
n<-10000
set.seed(123)
guesses<-runif(n, min=prior1, max=prior2)

df<-data.frame(id=1:10,
               Mean=runif(10, 110,120),
               raisedBP= runif(10, 0.1,0.11))

get.stdev<-function(mean,htn){
    data.frame(error=sqrt((htn-pgamma(q=140, shape=(mean/guesses)^2, scale=(guesses^2)/mean, lower.tail = F))^2),
               stdev = guesses)%>%filter(error == min(error))%>%pull(stdev)
}

mapply(get.stdev, df$Mean, df$raisedBP)

#check
test<-bind_cols(df, stdev=mapply(get.stdev, df$Mean, df$raisedBP))%>%
  mutate(htn2 = pgamma(q=140, shape=(Mean/stdev)^2, scale=(stdev^2)/Mean, lower.tail = F),
         error = 100*(raisedBP-htn2)/raisedBP)


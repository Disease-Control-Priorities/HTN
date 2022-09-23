rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, data.table, ggplot2, sf, rnaturalearth, rnaturalearthdata)

#Using age-standardized rates of htn control from 2000-2019

ncdr<-read.csv("NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv", stringsAsFactors = F)%>%
  select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
  rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
         Country = Country.Region.World)%>%
  group_by(Country, ISO, Year)%>%
  summarise(control = mean(control))%>%
  group_by(Country, ISO)%>%
  mutate(change = shift(control, type='lead')- control,
         r_change = 100*change/control)

##########
#https://www.statology.org/quadratic-regression-r/
#########
ncdr$control2<-ncdr$control^2
quadraticModel <- lm(change ~ control + control2, data=ncdr%>%filter(Country=="Canada"))
summary(quadraticModel)

#create sequence of control values
controlValues <- seq(0, 0.60, 0.01)
#create list of predicted change using quadratic model
changePredict <- predict(quadraticModel,list(control=controlValues, control2=controlValues^2))

data<-ncdr%>%filter(Country=="Canada")

fit<-data.frame(controlValues=controlValues, 
                changePredict=changePredict)

ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, aes(x=controlValues, y=changePredict))+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")

#Add custom fxn line
quadraticModel2<-quadraticModel
quadraticModel2$coefficients[1]<-0
quadraticModel2$coefficients[2]<-0.43*0.527
quadraticModel2$coefficients[3]<- -0.43
quadraticModel2

#create sequence of control values
controlValues2 <- seq(0, 0.60, 0.01)
#create list of predicted change using quadratic model
changePredict2 <- predict(quadraticModel2,
                          list(control=controlValues2, 
                               control2=controlValues2^2))

fit2<-data.frame(controlValues2=controlValues2, 
                changePredict2=changePredict2)

ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, 
            aes(x=controlValues, y=changePredict,color="Empirical"),
            size=1)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Modeled"),
            size=1)+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Empirical" = "darkblue", "Modeled" = "red"))


#ggsave("Canada.png", height = 6, width =8)

######
#compare to other countries
#####

more_data<-ncdr%>%filter(Country%in%c("Canada",
                                      "South Korea",
                                      "Germany",
                                      "Finland",
                                      "Iceland",
                                      "China"))

ggplot(more_data, aes(x=control, y=change))+
  geom_point(aes(colour=Country))+
  geom_line(data = fit, 
          aes(x=controlValues, y=changePredict),
          size=1, color="darkblue")+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2),
            size=1, color = "red")+
  xlab("Baseline control")+
  ylab("Change in coverage")+
  ylim(0,0.035)

#ggsave("other_countries.png", height = 6, width = 8)


########
#Ambitious
#######
quadraticModel3<-quadraticModel
quadraticModel3$coefficients[1]<-0
quadraticModel3$coefficients[2]<-0.285*0.75
quadraticModel3$coefficients[3]<- -0.285
quadraticModel3
#create sequence of control values
controlValues3 <- seq(0, 1, 0.01)
#create list of predicted change using quadratic model
changePredict3 <- predict(quadraticModel3,
                          list(control=controlValues3, 
                               control2=controlValues3^2))

fit3<-data.frame(controlValues3=controlValues3, 
                 changePredict3=changePredict3)


ggplot(data, aes(x=control, y=change))+
  geom_point()+
  geom_line(data = fit, 
            aes(x=controlValues, y=changePredict,color="Empirical"),
            size=1)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  xlab("Baseline control")+
  xlim(0,1.1)+
  ylab("Change in coverage")+
  ggtitle("Canada: 1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Empirical" = "darkblue", 
                                "Progress" = "red",
                                "Aspirational" = "darkgreen"))

#### Appendix plot####
ggplot(ncdr, aes(x=100*control, y=100*change))+
  geom_point()+
  geom_line(data = fit2, 
            aes(x=100*controlValues2, y=100*changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=100*controlValues3, y=100*changePredict3,color="Aspirational"),
            size=1)+
  xlab("Proportion of population with blood pressure controlled in year t (%)")+
  xlim(0,100)+
  ylim(0,5)+
  ylab("Additional proportion of population with blood pressure controlled in year t + 1 (%)")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "red",
                                "Aspirational" = "blue"))

#ggsave("figures/scale-up.png",height = 6, width = 10)

ggplot(ncdr, aes(x=Year, y=control))+
  geom_point()

mean(ncdr%>%filter(Year==2019)%>%arrange(control)%>%pull(control))
df<-ncdr%>%filter(Year==2019)%>%arrange(desc(control))
df<-df[1:5,]
mean(df$control)


############################
#add resolve data
###########################
rtsl<-read.csv("add_cov_data.csv", stringsAsFactors = F)%>%
  filter(location%in%c("Bangladesh", "Colombia", "Ecuador", "Ethiopia", "Peru", "Vietnam", "India"))

rtsl[8,4]<-"Longest running HEARTS \nprogram (year 1)"
rtsl[9,4]<-"Longest running HEARTS \nprogram (year 2)"

rtsl<-rtsl%>%mutate(data = ifelse(data=="RTSL", "HEARTS program", data))

plot<-bind_rows(rtsl, ncdr%>%mutate(data="NCD-RisC"))%>%arrange(desc(data))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "grey", "#0072B2", "#D55E00", "#CC79A7")

mytheme <- theme_bw() + theme(legend.title = element_blank())
theme_set(mytheme)

ggplot(plot%>%filter(change>=0), 
       aes(x=100*control, y=100*change, color=data, fill=data, 
           alpha=data, size=data, shape=data, linetype=data))+
  geom_point()+
  scale_fill_manual(values = c("#1E88E5", cbPalette[1],cbPalette[3],cbPalette[7],"black",'#D81B60'),
                    aesthetics = c("colour", "fill")) +
  scale_alpha_manual(values = c(1,1, 1,  1, 0.1, 1))+
  scale_size_manual(values = c(1,2,2,2,1, 1))+
  scale_shape_manual(values = c(NA, 24,23,15, 19, NA))+
  scale_linetype_manual(values = c("solid", NA,NA,NA,NA, "solid"))+
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  xlab("Proportion of population with blood pressure controlled in year t (%)")+
  xlim(0,100)+
  coord_cartesian(ylim=c(0,9))+
  ylab("Additional proportion of population with blood pressure controlled in year t+1 (%)")+
  #theme(legend.title=element_blank())+
  #theme_bw()+
  geom_line(data = fit2%>%filter(changePredict2>=0), 
            aes(x=100*controlValues2, y=100*changePredict2,
                color=   "Progress scenario \nscale-up function", 
                fill=    "Progress scenario \nscale-up function", 
                alpha=   "Progress scenario \nscale-up function",
                size=    "Progress scenario \nscale-up function", 
                shape =  "Progress scenario \nscale-up function", 
                linetype="Progress scenario \nscale-up function"))+
  geom_line(data = fit3%>%filter(changePredict3>=0), 
            aes(x=100*controlValues3, y=100*changePredict3,
            color=    "Aspirational scenario \nscale-up function", 
            fill=     "Aspirational scenario \nscale-up function", 
            alpha=    "Aspirational scenario \nscale-up function",
            size=     "Aspirational scenario \nscale-up function", 
            shape =   "Aspirational scenario \nscale-up function", 
            linetype= "Aspirational scenario \nscale-up function"))
  

ggsave("../../output/fig_A5.pdf", height = 6, width = 8, dpi=600)
  


#########bau############
#https://www.mathepower.com/en/quadraticfunctions.php

quadraticModel_1b<-quadraticModel
quadraticModel_1b$coefficients[1]<-0
quadraticModel_1b$coefficients[2]<-0.45*0.45
quadraticModel_1b$coefficients[3]<- -0.45

quadraticModel_2b<-quadraticModel
quadraticModel_2b$coefficients[1]<-0
quadraticModel_2b$coefficients[2]<-0.4*0.4
quadraticModel_2b$coefficients[3]<- -0.4

quadraticModel_3b<-quadraticModel
quadraticModel_3b$coefficients[1]<-0
quadraticModel_3b$coefficients[2]<-0.467*0.35
quadraticModel_3b$coefficients[3]<- -0.467

quadraticModel_4b<-quadraticModel
quadraticModel_4b$coefficients[1]<-0
quadraticModel_4b$coefficients[2]<-0.7*0.3
quadraticModel_4b$coefficients[3]<- -0.7

quadraticModel_5b<-quadraticModel
quadraticModel_5b$coefficients[1]<-0
quadraticModel_5b$coefficients[2]<-0.7*0.25
quadraticModel_5b$coefficients[3]<- -0.7

quadraticModel_6b<-quadraticModel
quadraticModel_6b$coefficients[1]<-0
quadraticModel_6b$coefficients[2]<-0.6*0.25
quadraticModel_6b$coefficients[3]<- -0.6

quadraticModel_7b<-quadraticModel
quadraticModel_7b$coefficients[1]<-0
quadraticModel_7b$coefficients[2]<-0.85*0.2
quadraticModel_7b$coefficients[3]<- -0.85

quadraticModel_8b<-quadraticModel
quadraticModel_8b$coefficients[1]<-0
quadraticModel_8b$coefficients[2]<-0.8*0.2
quadraticModel_8b$coefficients[3]<- -0.8

quadraticModel_9b<-quadraticModel
quadraticModel_9b$coefficients[1]<-0
quadraticModel_9b$coefficients[2]<-0.6*0.2
quadraticModel_9b$coefficients[3]<- -0.6


#create sequence of control values
controlValues <- seq(0, 0.6, 0.01)
#create list of predicted change using quadratic model
changePredict_1 <- predict(quadraticModel_1b,
                         list(control=controlValues, 
                              control2=controlValues^2))
changePredict_2 <- predict(quadraticModel_2b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_3 <- predict(quadraticModel_3b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_4 <- predict(quadraticModel_4b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_5 <- predict(quadraticModel_5b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_6 <- predict(quadraticModel_6b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_7 <- predict(quadraticModel_7b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_8 <- predict(quadraticModel_8b,
                           list(control=controlValues, 
                                control2=controlValues^2))
changePredict_9 <- predict(quadraticModel_9b,
                           list(control=controlValues, 
                                control2=controlValues^2))


fit_1<-data.frame(controlValues=controlValues, 
                    changePredict=changePredict_1)
fit_2<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_2)
fit_3<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_3)
fit_4<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_4)
fit_5<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_5)
fit_6<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_6)
fit_7<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_7)
fit_8<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_8)
fit_9<-data.frame(controlValues=controlValues, 
                  changePredict=changePredict_9)
######################################################
#fit each country to its BAU fxn - minimize errors
######################################################

ncdr1<-ncdr%>%mutate(fit1=predict(quadraticModel_1b,
                                    list(control=control, 
                                         control2=control^2)),
                     fit2=predict(quadraticModel_2b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit3=predict(quadraticModel_3b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit4=predict(quadraticModel_4b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit5=predict(quadraticModel_5b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit6=predict(quadraticModel_6b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit7=predict(quadraticModel_7b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit8=predict(quadraticModel_8b,
                                  list(control=control, 
                                       control2=control^2)),
                     fit9=predict(quadraticModel_9b,
                                  list(control=control, 
                                       control2=control^2)),
                     error1 = sqrt((change-fit1)^2),
                     error2 = sqrt((change-fit2)^2),
                     error3 = sqrt((change-fit3)^2),
                     error4 = sqrt((change-fit4)^2),
                     error5 = sqrt((change-fit5)^2),
                     error6 = sqrt((change-fit6)^2),
                     error7 = sqrt((change-fit7)^2),
                     error8 = sqrt((change-fit8)^2),
                     error9 = sqrt((change-fit9)^2)
                     )


ncdr1<-ncdr1%>%select(Country, ISO, Year, error1, error2, error3, error4, 
                      error5, error6, error7, error8, error9)%>%
  filter(Year>=2010)%>%
  gather(model, error, -ISO, -Year, -Country)%>%
  group_by(ISO,Country, model)%>%
  summarise(error = sum(error, na.rm = T))%>%
  ungroup()%>%
  group_by(ISO, Country)%>%
  filter(error==min(error))%>%
  mutate(model = substr(model, 6,6))


ncdr<-left_join(ncdr, ncdr1%>%select(ISO, model))

ggplot(ncdr, aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = 1), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_2%>%mutate(model = 2), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_3%>%mutate(model = 3), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_4%>%mutate(model = 4), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = 5), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_6%>%mutate(model = 6), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_7%>%mutate(model = 7), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_8%>%mutate(model = 8), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = 9), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  facet_wrap(~model)+
  xlab("Baseline control rate")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab("Change in HTN control coverage")+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "#D81B60",
                                "Aspirational" = "#1E88E5",
                                "BAU" = "#E4AB00"))+
  theme_bw()


#ggsave("baseline_fxns.png", height=8, width=12)
#take out countries already at 0.512 and make a 'progress=bau' bin

#######################################################
library(stringr)

ggplot(ncdr%>%filter(model%in%c(1,5,9))%>%mutate(model = factor(model, levels=c(1,5,9),
       labels=c("Best-performing quantile", "Middle-performing quantile", "Worst-performing quantile"))), 
       aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = "Best-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = "Middle-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = "Worst-performing quantile"), 
            aes(x=controlValues, y=changePredict,color="Business as usual scenario \nscale-up function"),
            size=1)+
  facet_wrap(~model, ncol=1)+
  xlab("Proportion of population with blood pressure controlled in year t (%)")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab(str_wrap("Additional proportion of population with blood pressure controlled in year t+1 (%)",45))+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress scenario \nscale-up function" = "#D81B60",
                                "Aspirational scenario \nscale-up function" = "#1E88E5",
                                "Business as usual scenario \nscale-up function" = "#E4AB00"))+
  theme_bw()


ggsave("../../output/fig_A6_alt.pdf", height=10, width=8, dpi=600)

#######################################################


ggplot(ncdr, aes(x=control, y=change))+
  geom_point(alpha=0.2)+
  geom_line(data = fit2, 
            aes(x=controlValues2, y=changePredict2,color="Progress"),
            size=1)+
  geom_line(data = fit3, 
            aes(x=controlValues3, y=changePredict3,color="Aspirational"),
            size=1)+
  geom_line(data = fit_1%>%mutate(model = 1), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_2%>%mutate(model = 2), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_3%>%mutate(model = 3), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_4%>%mutate(model = 4), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_5%>%mutate(model = 5), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_6%>%mutate(model = 6), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_7%>%mutate(model = 7), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_8%>%mutate(model = 8), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  geom_line(data = fit_9%>%mutate(model = 9), 
            aes(x=controlValues, y=changePredict,color="BAU"),
            size=1)+
  xlab("Baseline control rate")+
  xlim(0,1)+
  ylim(0,0.05)+
  ylab("Change in HTN control coverage")+
  ggtitle("1990-2019")+
  scale_color_manual(name = "Models", 
                     values = c("Progress" = "#D81B60",
                                "Aspirational" = "#1E88E5",
                                "BAU" = "#E4AB00"))+
  theme_bw()

  #ggsave("baseline_fxns_1.png", height=8, width=12)
#######################################################
#new fig
#######################################################

df<-read.csv("NCD-RisC_Lancet_2021_Hypertension_age_standardised_countries.csv", stringsAsFactors = F)%>%
  select(Country.Region.World, ISO, Year, Proportion.of.controlled.hypertension.among.all.hypertension)%>%
  #filter(Year>=2000)%>%
  rename(control = Proportion.of.controlled.hypertension.among.all.hypertension,
         Country = Country.Region.World)%>%
  group_by(Country, ISO, Year)%>%
  summarise(control = mean(control))%>%
  mutate(Progress = control,
         Aspirational = control,
         `Business as usual` = control,
         p_change = NA,
         a_change = NA,
         aroc = NA)

#for those already above say 49%, we assume the progress ~ bau
plocs<-df%>%filter(control>0.49)%>%pull(Country)%>%unique()

df<-left_join(df, ncdr1%>%select(Country, ISO, model))

for(i in 1:31){
  
  if(i>3){
  temp<-df%>%filter(Year==2018+i)%>%
    mutate(Year= Year+1,
           p_change = Progress*0.43*0.527 + (-0.43)*(Progress^2),
           a_change = Aspirational*0.285*0.75 + (-0.285)*(Aspirational^2),
           aroc = ifelse(model==1, `Business as usual`*0.45*0.45 + (-0.45)*(`Business as usual`)^2,
                         ifelse(model==2, `Business as usual`*0.4*0.4 + (-0.4)*(`Business as usual`)^2,
                                ifelse(model==3, `Business as usual`*0.467*0.35 + (-0.467)*(`Business as usual`)^2,
                                       ifelse(model==4, `Business as usual`*0.7*0.3 + (-0.7)*(`Business as usual`)^2,
                                              ifelse(model==5, `Business as usual`*0.7*0.25 + (-0.7)*(`Business as usual`)^2,
                                                     ifelse(model==6, `Business as usual`*0.6*0.25 + (-0.6)*(`Business as usual`)^2,
                                                            ifelse(model==7, `Business as usual`*0.85*0.2 + (-0.85)*(`Business as usual`)^2,
                                                                   ifelse(model==8, `Business as usual`*0.8*0.2 + (-0.8)*(`Business as usual`)^2,
                                                                          `Business as usual`*0.6*0.2 + (-0.6)*(`Business as usual`)^2)
                                                                   ))))))),
           aroc = ifelse(Country%in%plocs, p_change, aroc),
           p_change = ifelse(p_change<0,0,p_change),
           a_change = ifelse(a_change<0,0,a_change),
           aroc     = ifelse(aroc<0,0,aroc),
           Progress = Progress + p_change,
           Aspirational = Aspirational + a_change,
           `Business as usual` = `Business as usual` + aroc)
  }
  else{
    temp<-df%>%filter(Year==2018+i)%>%
      mutate(Year= Year+1,
             Progress = Progress,
             Aspirational = Aspirational,
             `Business as usual` = `Business as usual`,
             p_change = 0,
             a_change = 0,
             aroc     = 0)
  }
  df<-bind_rows(df, temp)
}

###########################################

regions<-read.csv("../Country_groupings_extended.csv", stringsAsFactors = F)%>%
  rename(ISO = iso3)%>%
  select(ISO, wb2021)

df<-left_join(df, regions)%>%
  mutate(`Business as usual` = ifelse(`Business as usual`>Progress, Progress, `Business as usual`))

plot<-df%>%select(Country, ISO, Year, wb2021, Progress, Aspirational, `Business as usual`)%>%
  gather(Scenario, control, - Country, - Year, - wb2021, -ISO)%>%ungroup()

plot$wb2021<-factor(plot$wb2021, levels = c("LIC", "LMIC", "UMIC", "HIC"))
plot$Scenario<-factor(plot$Scenario, levels = c("Business as usual", 
                                                "Progress",
                                                "Aspirational"))

##graph it
ggplot(na.omit(plot), 
       aes(x=Year, y=100*control, group = Country))+
  geom_line(size = 0.5)+
  facet_grid(wb2021~Scenario)+
  ylab("Hypertension control rate (%)")+
  geom_line(y=51.2, color="red", linetype='dotted')

ggsave("../../output/fig_A7.pdf", height = 8, width = 12, dpi=600)

write.csv(plot, "../../output/scale_up_data_2022.csv", row.names = F)

covfxn<-df%>%filter(Year>=2017)

covfxn$aroc[covfxn$Year<=2019]<-0
covfxn$p_change[covfxn$Year<=2019]<-0
covfxn$a_change[covfxn$Year<=2019]<-0

#write.csv(covfxn, "model/covfxn.csv", row.names = F)

####################
#plus salt
####################
#add salt impacts
#add salt impacts
data.in<-fread("../bp_data5.csv")%>%
  select(-Year, -Country)%>%
  rename(location = location_gbd,
         age = Age.group)
data.in$salt[data.in$location=="China"]<-4.83*2.52

names<-read.csv("../Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, iso3)%>%
  rename(location = location_gbd)

data.in<-left_join(data.in, names)

bpcats<-c("<120", "120-129", "130-139", 
          "140-149", "150-159", "160-169", 
          "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)

b_rates<-fread("../base_rates_2019.csv")%>%
  filter(cause=="ihd")%>%
  select(year, location, sex, age, Nx)%>%
  rename(Year=year)

source("../../model/functions_review_6.R")

repYear<-function(row){
  2017+floor((row-1)/224)
}

bp_out<-data.frame(Year=numeric(),
                   ref=numeric(),
                   asp=numeric(),
                   location=character()
)

data.in<-data.table(data.in)

for (i in unique(data.in$location)){
  DT<-unique(data.in[location==i][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_salt<-get.bp.prob(DT.in, 0.15, 'percent', 2023, 2030, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_salt2<-get.bp.prob(DT.in, 0.3, 'percent', 2023, 2027, 0, "baseline")
  DT.in<-DT[rep(seq(1,nrow(DT)), 24)][, Year:=repYear(.I)]
  bp_prob_base<-get.bp.prob(DT.in, 0, 'percent', 2023, 2025, 0, "baseline")
  setnames(bp_prob_base, "prob", "prob_0")
  setnames(bp_prob_salt2, "prob", "prob_2")
  
  bp_probs<-merge(bp_prob_salt, bp_prob_base, 
                  by=c("age","sex", "bp_cat", "Year", "location")) 
  bp_probs<-merge(bp_prob_salt2, bp_probs, 
                  by=c("age","sex", "bp_cat", "Year", "location")) 
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  bps<-left_join(bp_probs, b_rates%>%filter(location==i), 
                 by=c("age", "sex", "location", "Year"))
  
  normo<-bps%>%filter(bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139")%>%
    group_by(Year, age, sex, location)%>%
    summarise(propbase = sum(prob_0), 
              propref=sum(prob), 
              propasp=sum(prob_2),
              Nx=sum(Nx)/2)%>%
    ungroup()%>%group_by(Year, location)%>%
    summarise(base=weighted.mean(propbase, Nx),
              ref=weighted.mean(propref, Nx),
              asp=weighted.mean(propasp, Nx))%>%
    mutate(ref=ref-base, asp=asp-base)%>%
    select(Year, location, ref, asp)
  
  bp_out<-bind_rows(normo,bp_out)
  
}

bp_out<-left_join(bp_out, names)
#newdf<-left_join(df, bp_out%>%rename(ISO=iso3))

add<-merge(2041:2050, bp_out%>%filter(Year==2040)%>%
                 ungroup()%>%select(-Year))%>%rename(Year=x)

newdf<-bind_rows(bp_out, add)%>%filter(Year<=2050)


covfxn<-left_join(covfxn%>%rename(iso3 = ISO), newdf)

covfxn<-covfxn%>%
  mutate(reach_base = ifelse(0.512<=`Business as usual`, Year, NA),
         refwsalt = ifelse(0.512<=Progress+ref, Year, NA),
         aspwsalt = ifelse(0.512<=Aspirational+asp, Year, NA),
         reach_75 = ifelse(0.512<=Progress, Year, NA),
         reach_975 = ifelse(0.512<=Aspirational, Year, NA))%>%
  group_by(iso3)%>%
  mutate(reach_base = min(reach_base, na.rm=T),
         refwsalt = min(refwsalt, na.rm=T),
         aspwsalt = min(aspwsalt, na.rm=T),
         reach_75 = min(reach_75, na.rm=T),
         reach_975 = min(reach_975, na.rm=T))%>%
  ungroup()



covfxn<-covfxn%>%filter(location!="Global", 
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
                        location!="Virgin Islands, U.S.")

mean(covfxn$reach_75-covfxn$refwsalt)
mean(covfxn$reach_975-covfxn$aspwsalt)


##cumulative increases
covfxn<-covfxn%>%
  group_by(iso3)%>%
  mutate(p_change = Progress - control,
         p_change = ifelse(p_change<0,0, p_change),
         a_change = Aspirational - control,
         a_change = ifelse(a_change<0,0,a_change),
         aroc     = `Business as usual` - control,
         aroc     = ifelse(aroc<0,0,aroc))

covfxn$p_change[covfxn$Year<=2022]<-0
covfxn$a_change[covfxn$Year<=2022]<-0
covfxn$aroc[covfxn$Year<=2022]<-0

#update Jan 18 - scale-up relative to baseline
covfxn<-covfxn%>%
  group_by(iso3)%>%
  mutate(aroc2 = aroc/(1-control),
         p_change2 = p_change/(1-control),
         a_change2 = a_change/(1-control))


#add ideal - treat all htn in 2023
covfxn<-covfxn%>%mutate(ideal = ifelse(Year>=2023, (1-control), 0))


write.csv(covfxn, "../../model/covfxn2.csv", row.names = F)


################################
#maps for paper
################################
#setwd("~/RTSL")
df<-read.csv("../../model/covfxn2.csv", stringsAsFactors = F)
#df<-covfxn
df$reach_base[df$reach_base==Inf]<-2065
ssd<-df%>%filter(location =="Sudan")%>%
  mutate(iso3="SSD", location = "South Sudan")
df<-bind_rows(df, ssd)

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world<-left_join(world, df%>%rename(iso_a3 = iso3)%>%filter(Year==2050), by="iso_a3")
#write.csv(df, "web_appendix/shiny/map.csv", row.names = F)

world$achievebyb[world$reach_75<=2022]<-"Already achieved"
world$achievebyb[world$reach_75>2022 & world$reach_75<=2030]<-"2023-2030"
world$achievebyb[world$reach_75>2030 & world$reach_75<=2040]<-"2031-2040"
world$achievebyb[world$reach_75>2040 & world$reach_75<=2050]<-"2041-2050"
world$achievebyb[world$reach_75>2050]<-"After 2050"

world$achievebyb <- factor(world$achievebyb, levels = c("Already achieved", "2023-2030", "2031-2040",
                                                        "2041-2050","After 2050"))
any(world$reach_75>2050)

d<-ggplot(data = world) +
  geom_sf(aes(fill = factor(achievebyb))) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b"), 
                    drop=FALSE,
                    labels = c("Already achieved", "2023-2030", "2031-2040",
                               "2041-2050","After 2050"),
                    name= "Projected timeframe for \nachieving 80-80-80 target")+ 
  theme(legend.position = "right")+
  ggtitle("Progress scenario")

d
#ggsave("../../output/map_ref.png", height=6, width=10)

world$achievebyb2[world$reach_975<=2022]<-"Already achieved"
world$achievebyb2[world$reach_975>2022 & world$reach_975<=2030]<-"2023-2030"
world$achievebyb2[world$reach_975>2030 & world$reach_975<=2040]<-"2031-2040"
world$achievebyb2[world$reach_975>2040 & world$reach_975<=2050]<-"2041-2050"
world$achievebyb2[world$reach_975>2050]<-"After 2050"

world$achievebyb2 <- factor(world$achievebyb2, levels = c("Already achieved", "2023-2030", "2031-2040",
                                                          "2041-2050","After 2050"))
any(world$reach_975>2040)
any(world$reach_975>2050)

e<-ggplot(data = world) +
  geom_sf(aes(fill = factor(achievebyb2))) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b","#9B2226"), 
                    name= "Projected timeframe for \nachieving 80-80-80 target")+ 
  theme(legend.position = "right")+
  ggtitle("Aspirational scenario")

e

######################################################################

world$achievebybs[world$refwsalt<=2022]<-"Already achieved"
world$achievebybs[world$refwsalt>2022 & world$refwsalt<=2030]<-"2023-2030"
world$achievebybs[world$refwsalt>2030 & world$refwsalt<=2040]<-"2031-2040"
world$achievebybs[world$refwsalt>2040 & world$refwsalt<=2050]<-"2041-2050"
world$achievebybs[world$refwsalt>2050]<-"After 2050"

world$achievebybs <- factor(world$achievebybs, levels = c("Already achieved", "2023-2030", "2031-2040",
                                                          "2041-2050","After 2050"))

b<-ggplot(data = world) +
  geom_sf(aes(fill = factor(achievebybs))) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A", "#b9d780", "#feea83",
                               "#faa175", "#f8696b", "#9B2226"), 
                    name= "Projected timeframe for \nachieving 80-80-80 target")+ 
  theme(legend.position = "right")+
  ggtitle("Progress scenario")

#ggsave("figures/map_refwsalt.png", height=6, width=10)
b

world$achievebyb2s[world$aspwsalt<=2022]<-"Already achieved"
world$achievebyb2s[world$aspwsalt>2022 & world$aspwsalt<=2030]<-"2023-2030"
world$achievebyb2s[world$aspwsalt>2030 & world$aspwsalt<=2040]<-"2031-2040"
world$achievebyb2s[world$aspwsalt>2040 & world$aspwsalt<=2050]<-"2041-2050"
world$achievebyb2s[world$aspwsalt>2050]<-"After 2050"

world$achievebyb2s <- factor(world$achievebyb2s, levels = c("Already achieved", "2023-2030", "2031-2040",
                                                            "2041-2050","After 2050"))

c<-ggplot(data = world) +
  geom_sf(aes(fill = factor(achievebyb2s))) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b","#9B2226"), 
                    labels = c("Already achieved", "2023-2030", "2031-2040",
                               "2041-2050","After 2050"),
                    drop=FALSE,
                    name= "Projected timeframe for \nachieving 80-80-80 target")+ 
  theme(legend.position = "right")+
  ggtitle("Aspirational scenario")

#ggsave("figures/map_aspwsalt.png", height = 6, width=10)
c

world$achieveby00[world$reach_base<=2022]<-"Already achieved"
world$achieveby00[world$reach_base>2022 & world$reach_base<=2030]<-"2023-2030"
world$achieveby00[world$reach_base>2030 & world$reach_base<=2040]<-"2031-2040"
world$achieveby00[world$reach_base>2040 & world$reach_base<=2050]<-"2041-2050"
world$achieveby00[world$reach_base>2050]<-"After 2050"

world$achieveby00[world$reach_base>2050]

world$achieveby00 <- factor(world$achieveby00, levels = c("Already achieved", "2023-2030", "2031-2040",
                                                          "2041-2050","After 2050"))

leg<-world[1:6,]
leg$achieveby00<-factor(c("Already achieved", "2023-2030", "2031-2040",
                  "2041-2050","After 2050", NA),
                  levels = c("Already achieved", "2023-2030", "2031-2040",
                             "2041-2050","After 2050"))

library(ggpubr)

legend1<-get_legend(
  ggplot(data = leg) +
  geom_sf(aes(fill = factor(achieveby00))) +
  theme_bw()+
  theme(legend.position = 'right')+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#feea83",
                               "#faa175", "#f8696b","#9B2226"),
                    name = "Projected timeframe for \nachieving 80-80-80 target")
)

unique(world$achieveby00)
  
a<-ggplot(data = world) +
  geom_sf(aes(fill = factor(achieveby00))) +
  theme_bw()+
  scale_fill_manual(values = c("#2F635A","#b9d780", "#f8696b"))+
  ggtitle("Business as usual scenario")

a
#ggsave("figures/bizasusual.png", height = 6, width=10)

a<- a + theme(legend.position = 'none')
b<- b + theme(legend.position = 'none')
c<- c + theme(legend.position = 'none')
               
ggarrange(a, legend.grob=legend1, legend='right')
ggsave("../../output/map_business as usual.pdf", height = 6, width=10, dpi=300)
ggsave("../../output/map1.jpg", height = 6, width=10, dpi=1200)
ggsave( "../../output/map1.tiff", device='tiff', width=10, height=6)

ggarrange(b, legend.grob=legend1, legend='right')
ggsave("../../output/map_reference with salt.pdf", height = 6, width=10, dpi=300)
ggsave("../../output/map2.jpg", height = 6, width=10, dpi=1200)
ggsave( "../../output/map2.tiff", device='tiff', width=10, height=6)

ggarrange(c, legend.grob=legend1, legend='right')
ggsave("../../output/map_aspirational with salt.pdf", height = 6, width=10, dpi=300)
ggsave("../../output/map3.jpg", height = 6, width=10, dpi=1200)
ggsave( "../../output/map3.tiff", device='tiff', width=10, height=6)

ggarrange(d, legend.grob=legend1, legend='right')
ggsave("../../output/map_reference no salt.pdf", height = 6, width=10, dpi=300)
ggsave("../../output/map4.jpg", height = 6, width=10, dpi=1200)
ggsave( "../../output/map4.tiff", device='tiff', width=10, height=6)


ggarrange(e, legend.grob=legend1, legend='right')
ggsave("../../output/map_aspirational no salt.pdf", height = 6, width=10, dpi=300)
ggsave("../../output/map5.jpg", height = 6, width=10, dpi=1200)
ggsave( "../../output/map5.tiff", device='tiff', width=10, height=6)

dev.off()


a2<-ggarrange(a, legend.grob=legend1, legend='right')
library(cowplot)
plot_grid(a2, b, c, ncol=1, axis="l", greedy=FALSE, byrow=FALSE,
          rel_widths = c(1,0.8,0.8))
#cant


#########################

#how much faster on average with salt?
mean(df$reach_975-df$aspwsalt)
mean(df$reach_75-df$refwsalt)

max(df$aspwsalt)
max(df$refwsalt)


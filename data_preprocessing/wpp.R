
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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


    lifetab<-read.csv("WPP2019_Life_Table_Medium.csv", stringsAsFactors = F)%>%
      filter(MidPeriod>=2018 & MidPeriod<=2053)%>%filter(Sex!="Total")
    
    lifetab<-lifetab %>%select(-c(SexID, LocID, VarID,Variant))
    lifetab$Time2[lifetab$Time=="2015-2020" | lifetab$Time=="2020-2025"]<-2020
    lifetab$Time2[lifetab$Time=="2025-2030" | lifetab$Time=="2030-2035"]<-2030
    lifetab$Time2[lifetab$Time=="2035-2040" | lifetab$Time=="2040-2045"]<-2040
    lifetab$Time2[lifetab$Time=="2045-2050" | lifetab$Time=="2050-2055"]<-2050
    
    lifetab<-lifetab%>%group_by(Sex,AgeGrp, AgeGrpStart, AgeGrpSpan, Location, Time2)%>%
      summarize(mx=mean(mx), qx=mean(qx), px=mean(px), lx=mean(lx), dx=mean(dx), Lx=mean(Lx), Sx=mean(Sx),
                Tx=mean(Tx), ex=mean(ex), ax=mean(ax))
    
    unique(lifetab$Location)
    
    pop<-read.csv("WPP2019_PopulationBySingleAgeSex_2020-2100.csv", stringsAsFactors = F)%>%
      filter(Time==2020 | Time==2030 | Time==2040 | Time==2050)
    pop<-pop%>%select(Time,Location,AgeGrp,PopMale, PopFemale)%>%
      gather(Sex, pop, -Time, -Location, -AgeGrp)
    pop$Sex[pop$Sex=="PopMale"]<-"Male"
    pop$Sex[pop$Sex=="PopFemale"]<-"Female"
    names(pop)[1]<-"Time2"
    pop$pop<-pop$pop*1000
    
    pop1<-pop%>%filter(AgeGrp==0)
    pop1$AgeGrp<-"0"
    pop2<-pop%>%filter(AgeGrp>0 & AgeGrp<5)%>%group_by(Time2,Sex,Location)%>%summarize(pop=sum(pop))
    pop2$AgeGrp<-"1-4"
    pop3<-pop%>%filter(AgeGrp>=5 & AgeGrp<10)%>%group_by(Time2,Sex,Location)%>%summarize(pop=sum(pop))
    pop3$AgeGrp<-"5-9"
    pop4<-pop%>%filter(AgeGrp>=10 & AgeGrp<15)%>%group_by(Time2,Sex,Location)%>%summarize(pop=sum(pop))
    pop4$AgeGrp<-"10-14"
    pop5<-pop%>%filter(AgeGrp>=15 & AgeGrp<20)%>%group_by(Time2,Sex,Location)%>%summarize(pop=sum(pop))
    pop5$AgeGrp<-"15-19"    

    pop_u20<-bind_rows(pop1,pop2,pop3,pop4,pop5)    
    LT_u20<-left_join(lifetab, pop_u20, by=c("Sex", "Location", "Time2", "AgeGrp"))
    LT_u20$deaths<-LT_u20$pop*LT_u20$mx
    
    write.csv(LT_u20,"../model/WPP_LT2.csv", row.names = F)
    
    unique(LT_u20$Location)
    
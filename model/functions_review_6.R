
get.bp.prob<-function(DT, salteff, saltmet, saltyear1, saltyear2, rx, drugaroc){

if(rx==1 & drugaroc =="baseline"){
  DT[,covinc:=aroc2]
  #DT[,target_year:=ifelse(reach_base>2022, reach_base, 2022)]
}

if(rx==1 & drugaroc=="p75"){
  DT[,covinc:=p_change2]
  #DT[,target_year:=ifelse(refwsalt>2022, refwsalt, 2022)]
}
  
if(rx==1 & drugaroc=="p975"){
  DT[,covinc:=a_change2]
  #DT[,target_year:=ifelse(aspwsalt>2022, aspwsalt, 2022)]
}
  
if(rx==1 & drugaroc=="ideal"){
    DT[,covinc:=ideal]
    #DT[,target_year:=2030]
}
  
else{}

  
#make salt variable represent salt gap
if(saltmet=="percent"){
  DT[,salt_target:=salt*(1-salteff)]
  DT[salt_target<5.04, salt_target:=5.04]
  DT[salt<5.04, salt:=0]
  DT[salt>0,salt:=salt-salt_target]
  DT[salt<0, salt:=0]
  }

if(saltmet=="target"){
  DT[,salt:=salt-salteff]
  DT[salt<0, salt:=0]
}

if(saltmet=="app"){
  DT[,salt:=salteff]
}

else{}

if(salteff!=0){
DT[Year>=saltyear1 & Year<=saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt*(Year-saltyear1+1)/(saltyear2-saltyear1+1))]
DT[Year>saltyear2, Mean:=Mean-(((1.12*raisedBP)+((1-raisedBP)*0.58))*salt)]
}

else{}

DT[bp_cat=="<120", prob:=pnorm(120,Mean,stdev)]
DT[bp_cat=="120-129", prob:=pnorm(130,Mean,stdev)-pnorm(120,Mean,stdev)]
DT[bp_cat=="130-139", prob:=pnorm(140,Mean,stdev)-pnorm(130,Mean,stdev)]
DT[bp_cat=="140-149", prob:=pnorm(150,Mean,stdev)-pnorm(140,Mean,stdev)]
DT[bp_cat=="150-159", prob:=pnorm(160,Mean,stdev)-pnorm(150,Mean,stdev)]
DT[bp_cat=="160-169", prob:=pnorm(170,Mean,stdev)-pnorm(160,Mean,stdev)]
DT[bp_cat=="170-179", prob:=pnorm(180,Mean,stdev)-pnorm(170,Mean,stdev)]
DT[bp_cat=="180+", prob:=1-pnorm(180,Mean,stdev)]

if(rx==1){
  DT[,shift:=prob*(covinc)]
  DT[bp_cat=="<120" | bp_cat=="120-129" | bp_cat=="130-139", shift:=0]
  DT[, add130:=sum(shift*diabetes), by=.(age, sex, Year)]
  DT[, add140:=sum(shift*(1-diabetes)), by=.(age, sex, Year)]
  DT[,prob:=prob-shift]
  DT[bp_cat=="120-129", prob:=prob+add130]
  DT[bp_cat=="130-139", prob:=prob+add140]
}

else{}

DT[,c("age", "sex", "Year", "bp_cat" ,"prob", "location")]

}



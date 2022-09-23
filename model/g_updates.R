
gs<-c("Germany", "Ghana", "Grenada", "Greece", "Georgia")
###
progress2<-project.all("Germany", "percent", 0.15, 2030, "p75")
for(i in 2:5){
progress2<-bind_rows(progress2, project.all(gs[i], "percent", 0.15, 2030, "p75"))
}

aspirational2<-project.all("Germany", "percent",  0.3,  2027, "p975")
for(i in 2:5){
aspirational2<-bind_rows(aspirational2, project.all(gs[i], "percent",  0.3,  2027, "p975"))
}

##compare to original data
load("../model/model_output_newnorm.Rda")

progress2$version<-2
progress$version<-1
aspirational2$version<-2
aspirational$version<-1

test_plot<-bind_rows(progress2, progress%>%filter(location %in% gs))%>%
  group_by(location, year, intervention, version)%>%
  summarise(dead = sum(dead), sick=sum(sick))

ggplot(test_plot%>%filter(intervention=="Both"), 
       aes(x=year, y=dead, color=as.factor(version)))+
  geom_point()+
  facet_wrap(~location)

####
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function "Not In"

progress<-bind_rows(progress2, 
                    progress%>%filter(location %!in% gs))%>%
  select(-version)

aspirational<-bind_rows(aspirational2, 
                        aspirational%>%filter(location %!in% gs))%>%
  select(-version)


#####

rm(list = c("progress2", "aspirational2", "test_plot"))

##############################################################################################################

save.image(file = "../model/model_output_updated.Rda")

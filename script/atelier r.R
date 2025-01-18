
library("tidyverse")
library("ratdat")

#exploration de donné
?complete_old
summary(complete_old)
head(complete_old)
#data frame = vecteur / colonne / 
str(complete_old)

#ggplot
library(ggplot2)
#gg=grammar graphic
#R graph galary = voir graphique de la communauté
#ggplot(data="data"(ou), mapping=aes(variable/ quoi)+geom_function(comment))+ geom_function(ajout)) (aplha=transparence)
ggplot(complete_old,mapping = aes(x=weight,y=hindfoot_length, color= plot_type)) + geom_point(alpha=0.1 )
complete_old<-filter(complete_old, !is.na(weight), !is.na(hindfoot_length))
#on aime les couleurs
ggplot(complete_old,mapping = aes(x=weight,y=hindfoot_length, )) + geom_point(alpha=0.1, color= "darkorchid4" )

#forme->sex ( mieux de faire 2 graphiques différent pour mieux voir)
ggplot(complete_old,mapping = aes(x=weight,y=hindfoot_length, shape = sex)) + geom_point(alpha=0.1, color= "darkorchid4" )

#couleur=anné
ggplot(complete_old,mapping = aes(x=weight,y=hindfoot_length,color= year )) + geom_point(alpha=0.1  )

ggplot(complete_old,mapping = aes(x=weight,y=hindfoot_length, color= plot_type)) + geom_point(alpha=0.1 )+ 
  scale_color_viridis_d() + scale_x_log10()

#boxplot selon
ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length, color= plot_type)) + 
  geom_boxplot() + geom_jitter(alpha=0.1)+ scale_x_discrete(labels= label_wrap_gen(width=10)) 
#sans que boxplot change de couleur / outlier.shape= enlever les points à l'extreme
ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter(alpha=0.1, aes(color=plot_type))+ scale_x_discrete(labels= label_wrap_gen(width=10)) 
#on voit mal donc on met box plot apres / fill=na enlever le blanc du box

ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length)) + 
  geom_jitter(alpha=0.1, aes(color=plot_type))+geom_boxplot(outlier.shape = NA, fill=NA) +  scale_x_discrete(labels= label_wrap_gen(width=10)) 

#graphique en violon
ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length)) + 
  geom_jitter(alpha=0.1, aes(color=plot_type)) +geom_violin(fill= NA)+  scale_x_discrete(labels= label_wrap_gen(width=10)) 

#on reviens au boxplot avec les theme / bw = generique/ sans = precis comme legende/ titre des axes

ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length)) + 
  geom_jitter(alpha=0.1, aes(color=plot_type))+geom_boxplot(outlier.shape = NA, fill=NA) +  scale_x_discrete(labels= label_wrap_gen(width=10)) +
  theme_bw() + theme(legend.position = "none") + labs(x="plot type", y= "Hindfoot length(mm)")

#ajouter une autre variable comme le sex / ajout de facet_warp

plotfinale<-ggplot(complete_old,mapping = aes(x=plot_type,y=hindfoot_length)) + 
  geom_jitter(alpha=0.1, aes(color=plot_type))+geom_boxplot(outlier.shape = NA, fill=NA) +  scale_x_discrete(labels= label_wrap_gen(width=10)) +
  theme_bw() + facet_wrap(vars(sex, ncol=1))+
  theme(legend.position = "none") + labs(x="plot type", y= "Hindfoot length(mm)")

plotfinale
ggsave(filename = "figure/plotfinale.png", plot=plotfinale, height=6, width=8)

#tinyverse

surveys<- read.csv("data/brut-raw/surveys_complete_77_89.csv")
view(surveys)
str(surveys)
#select()=colonnes/filter()=ligne / mutate()=créer des colonnes/ groupby()
select(surveys, plot_id, species_id)
select(surveys, c(3,4)) #pas favorable car on sait pas lequel avec les codes
#sans plot id
select(surveys, -plot_id)

select(surveys, where(is.numeric))
select(surveys, where(anyNA))

#filter
filter(surveys, year==1988 )
filter(surveys, species_id %in% c("RM","DO"))
#donnée entre 1980 et 1985 et variable, year,month,species_id, plot_id / 3 facons de le faire
select(surveys, year,month,species_id, plot_id) + filter(surveys,year >=1980, year <=1985)
     
#1iere facons
surveys80<-filter(surveys,year >=1980 & year <=1985)
select(surveys80,  year,month,species_id, plot_id)
##2ieme facons
select(filter(surveys,year >=1980 & year <=1985),  year,month,species_id, plot_id)
##3ieme facons /crl+shift+m= %>%  (pipe)
surveys %>% filter(year==1980:1985) %>%  select(year,month,species_id, plot_id)

#données 1988, variable: reford_id, mas,species_id
surveys %>% filter(year==1988) %>%  select(record_id, month, species_id)

survey %>% filter (is.na(weight)) %>% mutate(weight_kg=weight/1000, weight_lbs=weight_kg*2.2) %>%
  relocate (weight_kg, .after=record_id) %>% relocate(weight_lbs, .after= weight_kg)

surveys %>% 
  mutate(date=paste(year, month,day,sep="-")) %>% 
  relocate(date, .after=year)
##marche pas lol
library(lubridate)  
surveys %>% 
  mutate(date=ymd (paste(year, month,day,sep="-")) %>% 
  relocate(date, .after=year)

surveys %>% 
  group_by(sex) %>% 
  summarize(mean.weight=mean(weight, na.rm=TRUE),count=n())
#exercice pouor le graph. nb d'observation par date selon male et 

surveys %>% mutate(date=ymd(paste(year, month,day,sep="-"))) %>% 
  relocate(date, .after=year) %>%  group_by(sex, date) %>% summarize(count=n()) %>%
    ggplot(aes(x=date,y=count,color=sex))+ geom_line()

  
  ###git###
###nouvelle section ###
#terminal changer pour git, nouveaux terminal...



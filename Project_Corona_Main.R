# Modern Regression Project

#Ejub Talovic & Santiago Anton 

# Last Update: 13.06.2020
#####################################################

library(mgcv)

library("AER")

source("basic_analysis.R")

set.seed(2)

####################################################



#Plots for our basic Exploratory data analysis.

par(mfrow=c(1,2))
plot.Country("France",plot=TRUE)
plot.Country("Germany",plot=TRUE)

par(mfrow=c(1,2))
plot.Country("United_States_of_America",plot=TRUE)
plot.Country("China",plot=TRUE)


###############################################
#Fitted Splines and slopes
first_day_china<-first_day_epidemic.death("China")
Death_china<-DeathsByCountry["China",]
n.days <- length(Death_china)
x <- c(1:n.days)
spline_fit_china<-gam(Death_china[first_day_china:n.days]~s(x[first_day_china:n.days]),family=poisson(link="log"))$fit
slope_china<-diff(diff(spline_fit_china))#Double derivative to find when slop stops increasing
slope_china #the initial phase lasted about 19 days


first_day_germany<-first_day_epidemic.death("Germany")
Death_germany<-DeathsByCountry["Germany",]
spline_fit_germany<-gam(Death_germany[first_day_germany:n.days]~s(x[first_day_germany:n.days]),family=poisson(link="log"))$fit
slope_germany<-diff(diff(spline_fit_germany))#Double derivative to find when slop stops increasing
slope_germany#around 22 negative values start.

first_day_france<-first_day_epidemic.death("France")
Death_france<-DeathsByCountry["France",]
spline_fit_france<-gam(Death_france[first_day_france:n.days]~s(x[first_day_france:n.days]),family=poisson(link="log"))$fit
slope_france<-diff(diff(spline_fit_france))
slope_france # 30 days initial phase

first_day_usa<-first_day_epidemic.death("United_States_of_America")
Death_usa<-DeathsByCountry["United_States_of_America",]
spline_fit_usa<-gam(Death_usa[first_day_usa:n.days]~s(x[first_day_usa:n.days]),family=poisson(link="log"))$fit
slope_usa<-diff(diff(spline_fit_usa))
slope_usa # 38 days initial phase



#################################################
#Poisson fitting for each country
germany.gam<-gam(Death_germany[first_day_germany:93]~x[first_day_germany:93],family=poisson(link="log"))
summary(germany.gam)
germany.gam$aic
dispersiontest(germany.gam,trafo=1) #overdispersion test there's no significant overdispersion in germany


china.gam<-gam(Death_china[first_day_china:40]~x[first_day_china:40],family = poisson(link="log"))
summary(china.gam) #less variance explained than germany
china.gam$aic
dispersiontest(china.gam,trafo = 1) # there is no significant evidence for overdispersion

france.gam<-gam(Death_france[first_day_france:94]~x[first_day_france:94],family = poisson())
summary(france.gam)
france.gam$aic #really bad aic
dispersiontest(france.gam,trafo = 1) # there is clearly overdispersion
france.gam2<-gam(Death_france[first_day_france:94]~x[first_day_france:94],family = quasipoisson())
summary(france.gam2) #the coefficient are the same but more confidence except the scale parameter

usa.gam<-gam(Death_usa[first_day_usa:100]~x[first_day_usa:100],family = poisson())
summary(usa.gam)
usa.gam$aic #auwfull aic
dispersiontest(usa.gam,trafo=1) #clearly overdispersion
usa.gam2<-gam(Death_usa[first_day_usa:100]~x[first_day_usa:100],family = quasipoisson())
summary(usa.gam2) #estimates are also the same


# Explication Santi le s(x) en gros c'est un smooth term mais le default c'est pas un natural cubic spline
#c'est un thin plate spline pas trop capté mais c'est un peu different du cours quoi

#en general un smooth term il fit la moyenne de maniere smooth et donc quand tu met x+s(x) tu as le smooth term
# qui fit la moyenne assez propre et le x sa le fit en mode c'est un poisson pour les petites variations autour
#donc je pense que si on veut l'exponentielle faut pas mettre le s(x) sinon on aura pas vraiment les coeff

#pour l'overdispersion on doit juste faire un quasipoisson au lieu de poisson sur la slide 112 du cours c'est mis 
#ça et sur le truc 

# Modern Regression Project

#Ejub Talovic & Santiago Anton 

# Last Update: 19.06.2020
#####################################################

library(mgcv)

library("AER")

library(boot)

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
slope_germany<-diff(diff(spline_fit_germany))
slope_germany#22 negative values start.

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

day_germany<-(x[first_day_germany:93]-first_day_germany)
germany.glm<-glm(Death_germany[first_day_germany:93]~day_germany,family=poisson(link="log"))
summary(germany.glm)
glm.diag.plots(germany.glm)
dispersiontest(germany.gam,trafo=1) #overdispersion test there's no significant overdispersion in germany
germany.beta<-germany.gam$coefficients[1]-log(CountryPop["Germany"]*1000000)

day_china<-(x[first_day_china:40]-first_day_china)
china.glm<-glm(Death_china[first_day_china:40]~day_china,family = poisson(link="log"))
summary(china.glm) 
glm.diag.plots(china.glm)
dispersiontest(china.gam,trafo = 1) # there is no significant evidence for overdispersion
china.beta<-china.gam$coefficients[1]-log(CountryPop["China"]*1000000)

day_france<-(x[first_day_france:87]-first_day_france)
france.glm<-glm(Death_france[first_day_france:87]~day_france,family = quasipoisson())
summary(france.glm)
glm.diag.plots(france.glm)
dispersiontest(france.glm,trafo = 1) # there is clearly overdispersion
france.beta<-france.glm$coefficients[1]-log(CountryPop["France"]*1000000)

day_usa<-(x[first_day_usa:89]-first_day_usa)
usa.glm<-glm(Death_usa[first_day_usa:89]~day_usa,family = quasipoisson())
summary(usa.glm)
glm.diag.plots(usa.glm,iden = T)
dispersiontest(usa.glm,trafo=1)
usa.beta<-usa.gam$coefficients[1]-log(CountryPop["United_States_of_America"]*1000000)


#################################################

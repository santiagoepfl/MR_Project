# Modern Regression Project

#Ejub Talovic & Santiago Anton 

# Last Update: 13.06.2020
#####################################################

library(mgcv)

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


#we need to find the end of the initial phase
#Fitted Splines and slopes
first_day_china<-first_day_epidemic.death("China")
Death_china<-DeathsByCountry["China",]
n.days <- length(Death_china)
x <- c(1:n.days)
# je pense sa fait pas beaucoup de sens de fit comme ça la spline
spline_fit_china<-gam(Death_china[first_day_china:n.days]~s(x[first_day_china:n.days]),family=poisson(link="log"))$fit
slope_china<-diff(diff(spline_fit_china))#Double derivative to find when slop stops increasing
slope_china


first_day_germany<-first_day_epidemic.death("Germany")

Death_germany<-DeathsByCountry["Germany",]
n.days<-length(Death_germany)
x <- c(1:n.days) 
spline_fit_germany<-gam(Death_germany[first_day_germany:n.days]~s(x[first_day_germany:n.days]),family=poisson(link="log"))$fit

# bon ducoup sa marche plus si on prend la bonne taille

slope_germany<-diff(diff(spline_fit_germany))#Double derivative to find when slop stops increasing
slope_germany#around 22 negative values start.
DeathsByCountry["Germany",]

#first_day_germany+22=92
germany.gam<-gam(Death_germany[first_day_germany:92]~x[first_day_germany:92],family=poisson(link="log"))
summary(germany.gam)


diff(diff(smooth.spline(x[first_day_germany:n.days],Death_germany[first_day_germany:n.days])$y))

smooth.spline(x[first_day_germany:n.days],Death_germany[first_day_germany:n.days])$y
first_day_France<-first_day_epidemic.death("France")

Death_France<-DeathsByCountry["France",]



first_day_USA<-first_day_epidemic.death("United_States_of_America")



##### Analysis after exploratory and initial phase finding
country<-"France"
y <- DeathsByCountry[country,]
m <- CasesByCountry[country,]
x <- c(1:n.days)
y
m#avec la mise a jour que j'ai faite y'a des cas negatif donc faudras gere ça

#avec le s, ca fait un bail de smooth mais il y quand même un rapport avec poisson/quasipoisson
gam(m~s(x),family=poisson(link="log"))
gam(m~s(x),family=poisson(link="log"))$fit
gam(m~s(x),family=poisson(link="log"))$smooth#les termes de smooth, rien compris.
gam(m~s(x),family=poisson(link="log"))$formula #ca veut pas donner les formules rip

plot(gam(m~s(x),family=poisson(link="log")))#apparemment ca fait le plot que quand il y a s(x), intéressant.


#je pense que c'est le machin de poisson normale avec summary, coefs et le trucs fitted
summary(gam(m~x,family=poisson(link="log")))
gam(m~x,family=poisson(link="log"))$coefficients
gam(m~x,family=poisson(link="log"))$fit


#E(Yt)=m*exp(alpha+beta*t) (attention ici m c'est la population totale, pas la response.)
#log(Yt)=log(m)+alpha+beta*t+g(t) (g(t) c'est l'éventuel smooth term,alpha intercep et beta c'est le coef de t)
#Je pense que mon machin en dessous c'est la bonne formule avec l'éventuel smooth term s(x) ou il faut mettre bonne option.
#On trouve le log(m)(m=population), on le soustrait à intercept et ça donne alpha vu que c'est constant
gam(m~x+s(x),family=poisson(link = "log"))

y

# Explication Santi le s(x) en gros c'est un smooth term mais le default c'est pas un natural cubic spline
#c'est un thin plate spline pas trop capté mais c'est un peu different du cours quoi

#en general un smooth term il fit la moyenne de maniere smooth et donc quand tu met x+s(x) tu as le smooth term
# qui fit la moyenne assez propre et le x sa le fit en mode c'est un poisson pour les petites variations autour
#donc je pense que si on veut l'exponentielle faut pas mettre le s(x) sinon on aura pas vraiment les coeff

#pour l'overdispersion on doit juste faire un quasipoisson au lieu de poisson sur la slide 112 du cours c'est mis 
#ça et sur le truc 

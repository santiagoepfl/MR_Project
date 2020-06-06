library(httr)

library(utils)

library(readxl)

deaths <- read_excel("GitHub/MR_Project/COVID-19-geographic-disbtribution-worldwide.xlsx")  # update file name (noting typo!) as necessary

# create names and labels for countries

CountryNames <- unlist(unique(deaths[,11]),use.names=F)  # in alphabetical order
CountryId <- match(CountryNames, unlist(deaths[,11],use.names=F))
# CountryPop <- match(CountryNames, unlist(deaths[,10]))/10^6  # 2018 populations in millions
# names(CountryPop) <- CountryNames

CountryId <- unlist(deaths[,7],use.names=F)[CountryId]
CountryId[CountryId=="UK"] <- "GB"      # country for UK is wrong
CountryId[CountryId=="EL"] <- "GR"      # ditto Greece
CountryId[CountryId=="PYF"] <- "PF"     # ditto French Polynesia
CountryId[CountryId=="NA"] <- "NA"      # Namibia is NA !

# CountryId <- CountryId[CountryId!=c("GE","JE","XK","JPG11668","AN")] 

nc <- length(CountryNames)

# days starting from and including 31 December 2019 (code will work OK up to end of 2020)

days.of.data <- 1 + (deaths$day + 31*(deaths$month>1) + 29*(deaths$month>2) + 31*(deaths$month>3) + 30*(deaths$month>4)
                     + 31*(deaths$month>5) + 30*(deaths$month>6) + 31*(deaths$month>7) + 31*(deaths$month>8) 
                     + 30*(deaths$month>9) + 31*(deaths$month>10) + 30*(deaths$month>11) )*(deaths$year==2020) 
n.days <- max(days.of.data)

# rounds a 3-day moving average of a vector of counts to the nearest integer

rounded.moving.average <- function(x, smooth=F)
{  # weight first and last days arbitrarily as (2,1)/3 and (1,2)/3, and the rest as (1,1,1)/3
  n <- length(x)
  z <- filter(x, filter=rep(1, 3)/3)  # leaves NAs as z[1] and z[n]
  z[1] <- (2*x[1]+x[2])/3  
  z[n] <- (x[n-1]+2*x[n])/3
  out <- round(z)
  if (!smooth) out <- x  # output original data if smooth=F
  out
}

# set up matrices with countries by rows and time as columns

DeathsByCountry <- matrix(0, nc, n.days, dimnames = list(CountryNames))
CasesByCountry <- matrix(0, nc, n.days, dimnames = list(CountryNames))

smooth.ts <- T  # change to F to analyse original time series, leave as T to smooth time series

for (i in 1:nc) 
{ i.days <- c(1:nrow(deaths))[deaths[,11]==CountryNames[i]]  # rows corresponding to country i

DeathsByCountry[i, days.of.data[i.days]] <- unlist(deaths[i.days, 6])
DeathsByCountry[i,] <- rounded.moving.average(DeathsByCountry[i,], smooth=smooth.ts)  # round to ensure integer numbers

CasesByCountry[i, days.of.data[i.days]] <- unlist(deaths[i.days, 5])
CasesByCountry[i,] <- rounded.moving.average(CasesByCountry[i,], smooth=smooth.ts)  # round to ensure integer numbers
}

# # get estimated national populations for 2020 for later comparison of rates 

pops <- read.csv(file="GitHub/MR_Project/CountryPopulations.csv", header=T)
CountryPop <- pops[match(CountryId,pops[,1]),3]/10^3
names(CountryPop) <- CountryNames

# drop `countries' with no data on population (Boat in Japan, Guernsey, Jersey, Kosovo, Namibia, Netherlands_Antilles, etc.)

droprow <- c(1:length(CountryPop))[is.na(CountryPop)]

DeathsByCountry <- DeathsByCountry[-droprow,]
CasesByCountry <- CasesByCountry[-droprow,]
CountryPop <- CountryPop[-droprow]
CountryNames <- CountryNames[-droprow]

# function for plotting (smoothed) country data, including

plot.Country <- function(country, names=CountryNames, deaths=DeathsByCountry, cases=CasesByCountry, pop=CountryPop, plot=F, plot.cumul=F, xmin=1, ...)
{
  out <- NULL
  n <- country
  pop <- pop[country]
  if (!is.character(country)) { n <- names[country]; pop <- pop[country] }
  if (plot) { 
    n.days <- length(deaths[country,])
    y <- deaths[country,]
    m <- cases[country,]
    x <- c(1:n.days)
    plot(m, main=n, xlab="Days since 31 December 2019", ylab="New cases", pch=16, cex=0.9, xlim=c(xmin,n.days), 
         panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);   abline(v=7*c(1:52), col="grey", lty=3) }, ...)
    lines(fitted(gam(m~s(x), family=quasipoisson(log))), col="red")
    plot(y, main=n, xlab="Days since 31 December 2019", ylab="Deaths", pch=16, cex=0.9, xlim=c(xmin,n.days), 
         panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);   abline(v=7*c(1:52), col="grey", lty=3) }, ...)
    lines(fitted(gam(y~s(x), family=quasipoisson(log))), col="red")
  }
  
  if(plot.cumul) { 
    n.days <- length(deaths[country,])
    y <- cumsum(deaths[country,])
    m <- cumsum(cases[country,])
    x <- c(1:n.days)
    sub.m <- x[m>0]
    sub.y <- x[y>0]
    plot(sub.m, m[sub.m]/pop, main=n, xlab="Days since 31 December 2019", ylab="Cumulative cases/deaths per million", pch=16, cex=0.9, xlim=c(xmin,n.days),
         log="y", ylim=10^c(-1,4), 
         panel.first = { abline(v=1+cumsum(c(1,31,29,31,30,31,30,31)), col="grey", lwd=1.5);   abline(v=7*c(1:52), col="grey", lty=3); 
           abline(h=10^c(-1:4), col="grey") }, ...)
    points(sub.y, y[sub.y]/pop, cex=0.9)
  }
}

#exemple avec France
country<-"France"
names=CountryNames
deaths=DeathsByCountry
cases=CasesByCountry
pop=CountryPop
n <- country
pop1 <- pop[country]
n.days <- length(deaths[country,])
y <- deaths[country,]
m <- cases[country,]
x <- c(1:n.days)


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


#Fonction pour premier jour de epidemie, je suis presque sûre qu'on en a besoin pour que le modèle marche.
first_day_epidemic.case<-function(country){
  n.days <- length(CasesByCountry[country,])
  y<-CasesByCountry[country,]
  n.days <- length(deaths[country,])
  x <- c(1:n.days)
  return(min(x[y>0]))
}

first_day_epidemic.death<-function(country){
  n.days <- length(DeathsByCountry[country,])
  y<-DeathsByCountry[country,]
  n.days <- length(deaths[country,])
  x <- c(1:n.days)
  return(min(x[y>0]))
}



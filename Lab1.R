###### Exercise 1 ######

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab1")

library(readxl)

EPI_data <- read.csv("epi2024results06022024.csv")
epi2024 <- read_excel("epi_2024_results_DAF24.xlsx")

View(EPI_data)
View(epi2024)

attach(EPI_data)
fix(EPI_data)

EPI.new
tf <- is.na(EPI.new)
E <- EPI.new[!tf]

summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)

stem(EPI.new)

hist(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)

boxplot(EPI.new, APO.new)

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ")) 
rug(EPI.new)

x<-seq(20,80,1) 
q<- dnorm(x,mean=42, sd=5,log=FALSE) 
lines(x,q)
lines(x,.4*q) 
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

###### Exercise 2 ######

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new); qqline(EPI.new)

qqplot(rnorm(ppoints(250), df = 5), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

qqplot(rt(ppoints(250), df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

###### Exercise 2a ######

###### BDH.new

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab1")
EPI_data <- read.csv("epi2024results06022024.csv")

attach(EPI_data)
fix(EPI_data)

tf <- is.na(BDH.new)
B <- BDH.new[!tf]

summary(BDH.new)
fivenum(BDH.new,na.rm=TRUE)

hist(BDH.new, seq(0., 90., 1.0), prob=TRUE) 
lines(density(BDH.new,na.rm=TRUE,bw=1.))
rug(BDH.new)

hist(BDH.new, seq(0., 90., 1.0), prob=TRUE)
lines (density(BDH.new,na.rm=TRUE,bw="SJ")) 
rug(BDH.new)

x<-seq(0,100,1) 
q<-dnorm(x,mean=52, sd=5,log=FALSE) 
lines(x,.12*q)

plot(ecdf(BDH.new), do.points=FALSE, verticals=TRUE) 

qqnorm(BDH.new); qqline(BDH.new)

qqplot(rnorm(ppoints(250)), BDH.new, xlab = "Q-Q plot for norm dsn") 
qqline(BDH.new)

qqplot(rt(ppoints(250), df = 5), BDH.new, xlab = "Q-Q plot for t dsn") 
qqline(BDH.new)

###### SPI.new

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab1")
EPI_data <- read.csv("epi2024results06022024.csv")

attach(EPI_data)
fix(EPI_data)

tf <- is.na(SPI.new)
S <- SPI.new[!tf]

summary(SPI.new)
fivenum(SPI.new,na.rm=TRUE)

hist(SPI.new, seq(0., 100., 1.0), prob=TRUE) 
lines(density(SPI.new,na.rm=TRUE,bw=1.))
rug(SPI.new)

hist(SPI.new, seq(0., 100., 1.0), prob=TRUE)
lines (density(SPI.new,na.rm=TRUE,bw="SJ")) 
rug(SPI.new)

x<-seq(0,100,1) 
q<-dnorm(x,mean=36, sd=5,log=FALSE)
lines(x,.12*q)
q<-dnorm(x,mean=70, sd=5,log=FALSE)
lines(x,.12*q)

plot(ecdf(SPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(SPI.new); qqline(SPI.new)

qqplot(rnorm(ppoints(250)), SPI.new, xlab = "Q-Q plot for norm dsn")
qqline(SPI.new)

qqplot(rt(ppoints(250), df = 5), SPI.new, xlab = "Q-Q plot for t dsn")
qqline(SPI.new)


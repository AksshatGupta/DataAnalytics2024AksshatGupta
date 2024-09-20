###### Exercise 1 ######

rm(list=ls())
setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab2")

EPI_data <- read.csv("epi2024results06022024.csv")
attach(EPI_data)

summary(EPI.new)

boxplot(EPI.new, APO.new)

qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

plot(ecdf(EPI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(EPI.new))

###### BDH.new

boxplot(BDH.new, EPI.new)

qqnorm(BDH.new)
qqline(BDH.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

plot(ecdf(BDH.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)
lines(ecdf(BDH.new))

qqplot(qchisq(ppoints(length(BDH.new)), df = 5), sort(BDH.new))
qqline(BDH.new, distribution = function(p) qchisq(p, df = 5))

###### CDA.new

boxplot(CDA.new, NDA.new)

qqnorm(CDA.new)
qqline(CDA.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)
qqplot(qnorm(ppoints(200)),CDA.new)
qqline(CDA.new)

plot(ecdf(CDA.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)
lines(ecdf(CDA.new))

qqplot(qbeta(ppoints(length(CDA.new)), shape1 = 2, shape2 = 5), sort(CDA.new))
qqline(CDA.new, distribution = function(p) qbeta(p, shape1 = 2, shape2 = 5))


###### Exercise 2 ######

rm(list=ls())

library(ggplot2)

setwd("C:/Users/akssh/Desktop/Fall2024/DA/Labs/Lab2")

EPI_data <- read.csv("epi2024results06022024.csv")
population_data <- read.csv("countries_populations_2023.csv")

merged_data <- merge(EPI_data, population_data, by.x = "country", by.y = "Country", all = FALSE)
merged_data <- merged_data[order(merged_data$country), ]

merged_data$population_log <- log(merged_data$Population)
lin.mod.epinew <- lm(EPI.new ~ population_log, data = merged_data)

plot(merged_data$population_log, merged_data$EPI.new)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)

ggplot(merged_data, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


###### Boxplots

boxplot(merged_data$GTI.new, merged_data$GTP.new)
boxplot(merged_data$GTP.new, merged_data$GHN.new)
boxplot(merged_data$GHN.new, merged_data$GTI.new)

###### Q-Q plots

qqplot(qchisq(ppoints(length(merged_data$GTI.new)), df = 5), sort(merged_data$GTI.new))
qqline(merged_data$GTI.new, distribution = function(p) qchisq(p, df = 5))

qqplot(qbeta(ppoints(length(merged_data$GTP.new)), shape1 = 2, shape2 = 5), sort(merged_data$GTP.new))
qqline(merged_data$GTP.new, distribution = function(p) qbeta(p, shape1 = 2, shape2 = 5))

qqplot(qweibull(ppoints(length(merged_data$GHN.new)), shape = 1.5), sort(merged_data$GHN.new))
qqline(merged_data$GHN.new, distribution = function(p) qweibull(p, shape = 1.5))

###### ECDF plots

plot(ecdf(merged_data$GTI.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)
lines(ecdf(merged_data$GTI.new))

plot(ecdf(merged_data$GTP.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 37, 10)), do.points=FALSE)
lines(ecdf(merged_data$GTP.new))

plot(ecdf(merged_data$GHN.new), do.points=FALSE)
plot(ecdf(rnorm(1000, 20, 15)), do.points=FALSE)
lines(ecdf(merged_data$GHN.new))

###### Summary and plots from 3 linear models

lin.mod.gtinew <- lm(GTI.new ~ population_log, data = merged_data)
summary(lin.mod.gtinew)
ggplot(merged_data, aes(x = population_log, y = GTI.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.gtinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

lin.mod.gtpnew <- lm(GTP.new ~ population_log, data = merged_data)
summary(lin.mod.gtpnew)
ggplot(merged_data, aes(x = population_log, y = GTP.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.gtpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

lin.mod.ghnnew <- lm(GHN.new ~ population_log, data = merged_data)
summary(lin.mod.ghnnew)
ggplot(merged_data, aes(x = population_log, y = GHN.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.ghnnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


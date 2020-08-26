# Aman Negassi
# Analysis of the Coronavirus Cases in the United States by Counties
# Professor Gavino
# CSC 499

library(dplyr)
library(ggplot2)
library(ISLR)
library(lattice)	

Coronavirus_Cases <- read.csv('us_counties.csv', header = T, stringsAsFactors = F)
names(Coronavirus_Cases)
table(Coronavirus_Cases$state) # Cases by state 

Rhode_Island <- subset(Coronavirus_Cases, Coronavirus_Cases$state == "Rhode Island")
Rhode_Island_TS<-ts(Rhode_Island$cases) # Series of counts, indexes of cases 0-500 cases daily 
plot(Rhode_Island_TS)

Rhode_Island_TS_diff<-diff(Rhode_Island_TS)
plot(Rhode_Island_TS_diff) # difference between the current day and the next day
#acf(Rhode_Island_TS_diff) # autocorrelation function - find the relationship between 2 variables E(y_t|y_t-1) times 0 
#pacf(Rhode_Island_TS_diff) # ?

Coronavirus_Cases.Rhode_Island = Coronavirus_Cases %>% filter(state == 'Rhode Island')
plot(as.Date(Coronavirus_Cases.Rhode_Island$date, format = "%m/%d/%y"), Coronavirus_Cases.Rhode_Island$cases, type = "l") # Total Cases of the whole state
lines(as.Date(Coronavirus_Cases.Rhode_Island$date, format = "%m/%d/%y"), Coronavirus_Cases.Rhode_Island$deaths, col="red")
summary(Coronavirus_Cases.Rhode_Island)
# Trend for Rhode Island Cases Absolute 
#Coronavirus_Cases.Rhode_Island$trend <- 1:80
#mod <- lm(cases~trend, data = Coronavirus_Cases.Rhode_Island)
#summary(mod)
#plot(mod) # slowly decreasing overall, distribution is skewed normal if it lies along the qq plot line
#lines(as.Date(Coronavirus_Cases.Rhode_Island$date, format = "%m/%d/%y"), Coronavirus_Cases.Rhode_Island$deaths, col="red")
# Trend for Rhode Island Cases Relative
#mod <- lm(cases/1059361 ~ trend, data = Coronavirus_Cases.Rhode_Island)
#summary(mod)
#lines(as.Date(Coronavirus_Cases.Rhode_Island$date, format = "%m/%d/%y"), Coronavirus_Cases.Rhode_Island$)
# Trend for Rhode Island Deaths Absolute
#deaths_Rhode_Island = cumsum(Coronavirus_Cases.Rhode_Island$deaths)

# Trend for Rhode Island Deaths Relative 
#mod_RI_deaths <- lm(deaths_Rhode_Island/1059361 ~ trend, data = Coronavirus_Cases.Rhode_Island)
#summary(mod_RI_deaths)
#lines(as.Date(Coronavirus_Cases.Rhode_Island$date, format = "%m/%d/%y"), mod_RI_deaths$fitted.values, col="blue", lwd=2)

# Providence County 
Coronavirus_Cases.RI.PVD <- Coronavirus_Cases %>% filter(state == 'Rhode Island', county=='Providence')
plot(as.Date(Coronavirus_Cases.RI.PVD$date, format="%m/%d/%y"), Coronavirus_Cases.RI.PVD$cases, type="l", xlab ="Time", ylab = "cases")
lines(as.Date(Coronavirus_Cases.RI.PVD$date, format = "%m/%d/%y"), Coronavirus_Cases.RI.PVD$deaths, col="red")

# Trend for Provience County Cases Absolute
Coronavirus_Cases.RI.PVD$trend <- 1:80
mod <- lm(cases ~ trend, data = Coronavirus_Cases.RI.PVD)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.PVD$date, format = "%m/%d/%y"), mod$fitted.values, col="green", lwd=2)
# Trend for Providence County cases Relative
mod <- lm(cases/638931 ~ trend, data = Coronavirus_Cases.RI.PVD)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.PVD$date, format = "%m/%d/%y"), mod$fitted.values, col="purple", lwd=2)
# Trend for Providence County deaths Absolute
deaths_Providence <- cumsum(Coronavirus_Cases.RI.PVD$deaths)
mod <- lm(deaths_Providence ~ trend, data = Coronavirus_Cases.RI.PVD)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.PVD$date, format = "%m/%d/%y"), mod$fitted.values, col = "yellow")
# Trend for Providence County deaths Relative
mod_deaths <- lm(deaths_Providence/638931 ~ trend, data = Coronavirus_Cases.RI.PVD)
summary(mod_deaths)
lines(as.Date(Coronavirus_Cases.RI.PVD$date, format = "%m/%d/%y"), mod_deaths$fitted.values, col="blue", lwd=2)

# Bristol County
Coronavirus_Cases.RI.Bristol <- Coronavirus_Cases %>% filter(state == 'Rhode Island', county=='Bristol')
plot(as.Date(Coronavirus_Cases.RI.Bristol$date, format="%m/%d/%y"), Coronavirus_Cases.RI.Bristol$cases, type="l", xlab ="Time", ylab = "cases")
lines(as.Date(Coronavirus_Cases.RI.Bristol$date, format = "%m/%d/%y"), Coronavirus_Cases.RI.Bristol$deaths, col="red")

# Trend for Bristol County Cases Absolute
Coronavirus_Cases.RI.Bristol$trend <- 1:80
mod = lm(cases ~ trend, data = Coronavirus_Cases.RI.Bristol)  # The slope is in absolute terms 
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Bristol$date, format = "%m/%d/%y"), mod$fitted.values, col="green", lwd=2)
# Trend for Bristol County Cases Relative
mod <- lm(cases/48479 ~ trend, data = Coronavirus_Cases.RI.Bristol)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Bristol$date, format = "%m/%d/%y"), mod$fitted.values, col = "blue", lwd = 2)
# Trend for Bristol County Deaths Absolute
deaths_Bristol <- cumsum(Coronavirus_Cases.RI.Bristol$deaths)
mod_deaths <- lm(deaths_Bristol ~ trend, data = Coronavirus_Cases.RI.Bristol)
summary(mod_deaths)
lines(as.Date(Coronavirus_Cases.RI.Bristol$date, format = "%m/%d/%y"), mod_deaths$fitted.values, col="orange", lwd=2)
# Trend for Bristol County Deaths Relative
mod_deaths2 <- lm(deaths_Bristol/48479 ~ trend, data = Coronavirus_Cases.RI.Bristol)
summary(mod_deaths2)
lines(as.Date(Coronavirus_Cases.RI.Bristol$date, format = "%m/%d/%y"), mod_deaths2$fitted.values, col="yellow", lwd=2)

# Kent Couny 
Coronavirus_Cases.RI.Kent = Coronavirus_Cases %>% filter(state == 'Rhode Island', county=='Kent')
plot(as.Date(Coronavirus_Cases.RI.Kent$date, format="%m/%d/%y"), Coronavirus_Cases.RI.Kent$cases, type="l", xlab ="Time", ylab = "cases")
lines(as.Date(Coronavirus_Cases.RI.Kent$date, format = "%m/%d/%y"), Coronavirus_Cases.RI.Kent$deaths, col="red")

# Trend for Kent County Cases Absolute
Coronavirus_Cases.RI.Kent$trend <- 1:80
mod <- lm(cases ~ trend, data = Coronavirus_Cases.RI.Kent)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Kent$date, format = "%m/%d/%y"), mod$fitted.values, col="green", lwd=2)
# Trend for Kent County Cases Relative
mod <- lm(cases/164292 ~ trend, data = Coronavirus_Cases.RI.Kent)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Kent$date, format = "%m/%d/%y"), mod$fitted.values, col = "yellow", lwd = 2)
# Trend for Kenty County Deaths Absolute
deaths_Kent <- cumsum(Coronavirus_Cases.RI.Kent$deaths)
mod <- lm(deaths_Kent ~ trend, data = Coronavirus_Cases.RI.Kent)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Kent$date, format = "%m/%d/%y"), mod$fitted.values, col = "orange")
# Trend for Kent County Deaths Relative
mod_deaths3 <- lm(deaths_Kent/164292 ~ trend, data = Coronavirus_Cases.RI.Kent)
summary(mod_deaths3)
lines(as.Date(Coronavirus_Cases.RI.Kent$date, format = "%m/%d/%y"), mod_deaths3$fitted.values, col="blue", lwd=2)

# Newport County
Coronavirus_Cases.RI.Newport <- Coronavirus_Cases %>% filter(state == 'Rhode Island', county=='Newport')
plot(as.Date(Coronavirus_Cases.RI.Newport$date, format="%m/%d/%y"), Coronavirus_Cases.RI.Newport$cases, type="l", xlab ="Time", ylab = "cases")
lines(as.Date(Coronavirus_Cases.RI.Newport$date, format = "%m/%d/%y"), Coronavirus_Cases.RI.Newport$deaths, col="red")

# Trend for Newport County Cases Absolute
Coronavirus_Cases.RI.Newport$trend <- 1:80
mod <- lm(cases ~ trend, data = Coronavirus_Cases.RI.Newport)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Newport$date, format = "%m/%d/%y"), mod$fitted.values, col="green", lwd=2)
# Trend for Newport County Cases Relative 
mod <- lm(cases/82082 ~ trend, data = Coronavirus_Cases.RI.Newport)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Newport$date, format = "%m/%d/%y"), mod$fitted.values, col="orange", lwd=2)
# Trend for Newport County Deaths Absolute 
deaths_Newport <- cumsum(Coronavirus_Cases.RI.Newport$deaths)
mod <- lm(deaths_Newport ~ trend, data = Coronavirus_Cases.RI.Newport)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Newport$date, format = "%m/%d/%y"), mod$fitted.values, col="yellow", lwd=2)
# Trend for Newport County Deaths Relative
mod_deaths4 <- lm(deaths_Kent/82082 ~ trend, data = Coronavirus_Cases.RI.Newport)
summary(mod_deaths4)
lines(as.Date(Coronavirus_Cases.RI.Newport$date, format = "%m/%d/%y"), mod_deaths4$fitted.values, col="blue", lwd=2)

# Washington County 
Coronavirus_Cases.RI.Washington <- Coronavirus_Cases %>% filter(state == 'Rhode Island', county=='Washington')
plot(as.Date(Coronavirus_Cases.RI.Washington$date, format="%m/%d/%y"), Coronavirus_Cases.RI.Washington$cases, type="l", xlab ="Time", ylab = "cases")
lines(as.Date(Coronavirus_Cases.RI.Washington$date, format = "%m/%d/%y"), Coronavirus_Cases.RI.Washington$deaths, col="red")

# Trend for Washington County Cases Absolute 
Coronavirus_Cases.RI.Washington$trend <- 1:80
mod <- lm(cases ~ trend, data = Coronavirus_Cases.RI.Washington)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Washington$date, format = "%m/%d/%y"), mod$fitted.values, col="green", lwd=2)
# Trend for Washington County Cases Relative
mod <- lm(cases/125577 ~ trend, data = Coronavirus_Cases.RI.Washington)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Washington$date, format = "%m/%d/%y"), mod$fitted.values, col="blue", lwd=2)
# Trend for Washington County Deaths Absolute
deaths_Washington <- cumsum(Coronavirus_Cases.RI.Washington$deaths)
mod <- lm(deaths_Washington ~ trend, data = Coronavirus_Cases.RI.Washington)
summary(mod)
lines(as.Date(Coronavirus_Cases.RI.Washington$date, format = "%m/%d/%y"), mod$fitted.values, col="yellow", lwd=2)
# Trend for Washington County Deaths Relative
mod_deaths5 <- lm(deaths_Washington/125577 ~ trend, data = Coronavirus_Cases.RI.Washington)
summary(mod_deaths5)
lines(as.Date(Coronavirus_Cases.RI.Washington$date, format = "%m/%d/%y"), mod_deaths5$fitted.values, col="orange", lwd=2)

# Time series for the counties deaths
Bristol<-subset(Rhode_Island,Rhode_Island$county=="Bristol")
Kent<-subset(Rhode_Island,Rhode_Island$county=="Kent")
Newport<-subset(Rhode_Island,Rhode_Island$county=="Newport")
Providence<-subset(Rhode_Island,Rhode_Island$county=="Providence")
Washington<-subset(Rhode_Island,Rhode_Island$county=="Washington")
ts.plot(data.frame(Bristol=Bristol$deaths,
                   Kent=Kent$deaths,Newport=Newport$deaths,
                   Providence=Providence$deaths,
                   Washington=Washington$deaths), gpars=list(col=rainbow(5)))
legend("topleft", legend=c("Bri","Ken","New"
                           ,"Prov","Wash"),col=rainbow(5), lty=1)

# Time Series for the counties cases
Bristol_cases <-subset(Rhode_Island,Rhode_Island$county=="Bristol")
Kent_cases<-subset(Rhode_Island,Rhode_Island$county=="Kent")
Newport_cases <-subset(Rhode_Island,Rhode_Island$county=="Newport")
Providence_cases <-subset(Rhode_Island,Rhode_Island$county=="Providence")
Washington_cases <-subset(Rhode_Island,Rhode_Island$county=="Washington")
ts.plot(data.frame(Bristol_cases=Bristol$cases,
                   Kent_cases=Kent$cases,Newport_cases=Newport$cases,
                   Providence_cases=Providence$cases,
                   Washington_cases=Washington$cases), gpars=list(col=rainbow(5)))
legend("topleft", legend=c("Bri","Ken","New"
                           ,"Prov","Wash"),col=rainbow(5), lty=1)

#Cumulative deaths 
Bristol_deaths <- Coronavirus_Cases.Rhode_Island %>% filter(county =='Bristol') %>%  group_by(date) %>% summarise(tot_daily_deaths = sum(deaths)) 
ggplot(Bristol_deaths, aes(x = as.Date(date, format = "%m/%d/%Y"), y = tot_daily_deaths)) + geom_line() + xlab("Time")
summary(Bristol_deaths)

Kent_deaths <- Coronavirus_Cases.Rhode_Island %>% filter(county =='Kent') %>%  group_by(date) %>% summarise(tot_daily_deaths = sum(deaths)) 
ggplot(Kent_deaths, aes(x = as.Date(date, format = "%m/%d/%Y"), y = tot_daily_deaths)) + geom_line() + xlab("Time")
summary(Kent_deaths)

Newport_deaths <- Coronavirus_Cases.Rhode_Island %>% filter(county =='Newport') %>%  group_by(date) %>% summarise(tot_daily_deaths = sum(deaths))
ggplot(Newport_deaths, aes(x = as.Date(date, format = "%m/%d/%Y"), y = tot_daily_deaths)) + geom_line() + xlab("Time")
summary(Newport_deaths)

Providence_deaths <- Coronavirus_Cases.Rhode_Island %>% filter(county =='Providence') %>%  group_by(date) %>% summarise(tot_daily_deaths = sum(deaths)) 
ggplot(Providence_deaths, aes(x = as.Date(date, format = "%m/%d/%Y"), y = tot_daily_deaths)) + geom_line() + xlab("Time")
summary(Providence_deaths)

Washington_deaths <- Coronavirus_Cases.Rhode_Island %>% filter(county =='Washington') %>%  group_by(date) %>% summarise(tot_daily_deaths = sum(deaths))
ggplot(Washington_deaths, aes(x = as.Date(date, format = "%m/%d/%Y"), y = tot_daily_deaths)) + geom_line() + xlab("Time")
summary(Washington_deaths)

#ts.plot(data.frame(Bristol_deaths=Bristol_deaths,
#                   Kent_deaths=Kent_deaths,Newport_cases=Newport_deaths,
#                   Providence_deaths=Providence_deaths,
#                   Washington_deaths=Washington_deaths), gpars=list(col=rainbow(5)))
#legend("topleft", legend=c("Bri","Ken","New"
#                           ,"Prov","Wash"),col=rainbow(5), lty=1)

# Excel
# Rate of growths for cases and deaths 
# Rows for counties and columns for absolute and relative cases and deaths

#rate of growth cases and deaths
# Find out which county is growing the fast in absolute terms and per capita

# Trend for the deaths

# Providence deaths by county / Pop of Rhode Island 
# Divide deaths by county population
# Population of RI deaths / Pop of Rhode Island 

# For trend, divide cases by population of that county 

# delay - today's case 500 cases y_

# y = beta_0 + beta_1x + epsilon
# y_t = alpha + beta*y_(t-1)
# p-values: 1.973e-15

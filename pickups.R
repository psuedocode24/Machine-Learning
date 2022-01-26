## pickup truck regression
## 
## this data are price, mileage, year, and brand for
## pickups from craigslist.

## read the data
pickups <- read.csv("pickups.csv")
 
xx <- seq(500, 30000, length=1000)
plot(xx, dnorm(xx,mean(pickups$price), sd(pickups$price)))

## explore a bit
nrow(pickups) # sample size
pickups[1,] # the first observation
pickups[1:3,] # the first three observations
pickups[1:3,1] # the first variable (year)
pickups$year[1:3] # same thing
pickups[1:3,'year'] # same thing again
summary(pickups) # summary of each variable
pickups[pickups$miles>200000,] # some real clunkers

## the make variable is special
class(pickups$make) # it is a factor (categorical)
levels(pickups$make) # with 3 levels
pickups$make[1:2] # the first two obs are GMC and Dodge
as.numeric(pickups$make[1:2]) # which R calls levels 3 and 1

## plots
hist(pickups$price) ## a histogram
plot(price ~ make, data=pickups) ## a boxplot
plot(price~miles, data=pickups) ## simple scatterplot
plot(price~miles, data=pickups, log="y") ## price on log scale
plot(price~miles, data=pickups, log="y", col=pickups$make) ## in color
## add a legend (colors 1,2,3 are black,red,green)
legend("topright", fill=1:3, legend=levels(pickups$make))

## regressions: write these models out, please make sure you understand them

## main effects, regression of log price on miles, year, make
## 'glm' is generalized linear model; this is your least-squares regression
fit_maineffects <- glm(log(price) ~ make + miles + year, data=pickups) # ~. means 'on everything'
## summarize the regression model
summary(fit_maineffects)
## what is the 95% CI for effect of miles on log price?
## why are there only two brand effects?

## interaction: allow mileage depreciation to depend upon make.
## in glm formulas, '+' means 'and', '*' means 'interacted with'
fit_interact <- glm(log(price) ~ year + miles*make, data=pickups) 
summary(fit_interact)
## what is the mileage depreciation rate for each of Dodge, Ford, and GMC?



## House Prices in Boston

library(MASS)
data <- Boston

# This data frame contains the following columns:
# 
# crim	per capita crime rate by town.
# zn		proportion of residential land zoned for lots over 25,000 sq.ft.
# indus	proportion of non-retail business acres per town.
# chas	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
# nox		nitrogen oxides concentration (parts per 10 million).
# rm		average number of rooms per dwelling.
# age		proportion of owner-occupied units built prior to 1940.
# dis		weighted mean of distances to five Boston employment centres.
# rad		index of accessibility to radial highways.
# tax		full-value property-tax rate per $10,000.
# ptratio	pupil-teacher ratio by town.
# black	1000(Bk-0.63)^2 where Bk is the proportion of blacks by town.
# lstat	lower status of the population (percent).
# medv	median value of owner-occupied homes in $1000s.


# let's look at the linear model first
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv ~ ., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
mse.lm <- sum((pr.lm - test$medv)^2) / nrow(test)
mse.lm


# scaling
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]


# NN
library(neuralnet)

nn <- neuralnet(medv ~ ., data=train_, hidden=c(5,3), linear.output=T)
plot(nn)

pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
mse.nn <- sum((test.r - pr.nn_)^2) / nrow(test_)


# compare LM and NN
print(paste(mse.lm, mse.nn))

# visualize
par(mfrow=c(1,2))
plot(test$medv,pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(0,1, lwd=2)
legend('bottomright', legend='NN',pch=18,col='red', bty='n')
plot(test$medv,pr.lm, col='blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0,1, lwd=2)
legend('bottomright', legend='LM', pch=18, col='blue', bty='n', cex=.95)

# more visualization
plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
points(test$medv, pr.lm, col='blue', pch=18, cex=0.7)
abline(0, 1, lwd=2)
legend('bottomright', legend=c('NN','LM'), pch=18, col=c('red','blue'))

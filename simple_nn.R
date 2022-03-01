
## Simple Example in R

library(neuralnet)

# creating training data set
tks <- c(20,10,30,20,80,30) # technical knowledge score
css <- c(90,20,40,50,50,80) # communication skills score
placed <- c(1,0,0,0,1,1)

df=data.frame(tks, css, placed)

# trains neural network using backpropagation
nn <- neuralnet(placed ~ tks + css, data=df, hidden=3, act.fct="logistic",
             linear.output=F)

# placed ~ tks + css: placed is label and tks and css are features.
# hidden=3: represents single layer with 3 neurons respectively.
# act.fct="logistic": activation function, "logistic" or "tanh"
# linear.ouput=FALSE: set FALSE for apply act.fct otherwise TRUE


# plot neural network
plot(nn)


# creating test set
test.tks <- c(30,40,85)
test.css <- c(85,50,40)
test <- data.frame(test.tks, test.css)


# Prediction using neural network
predict <- compute(nn,test)
predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
ifelse(predict$net.result>0.5, 1, 0)


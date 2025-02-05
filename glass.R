### Forensic Glass ###

library(MASS) ## a library of example datasets
data(fgl) ## loads the data into R; see help(fgl)
par(mfrow=c(2,3))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6))

## for illustration, consider the RIxMg plane (i.e., just 2D)

## make numerica and scale
## convert columns to mean-zero sd-one
x <- scale(fgl[,c("RI","Mg")])
## check that we've standardized
apply(x,2,sd) # see ?apply
apply(x,2,mean)
## pull out glass type
gtype <- fgl$type
n <- length(gtype)

## use 200 training points to find nearest neighbors for 14
train <- sample(1:214,200)

library(class)
nearest1 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=1)
nearest5 <- knn(train=x[train,], test=x[-train,], cl=gtype[train], k=5)
data.frame(gtype[-train],nearest1,nearest5)

## plot them to see how it worked
par(mfrow=c(1,2))
plot(x[train,], col=gtype[train], cex=.8, main="1-nearest neighbor")
points(x[-train,], bg=nearest1, pch=21, col=grey(.9), cex=1.25)
plot(x[train,], col=gtype[train], cex=.8, main="5-nearest neighbors")
points(x[-train,], bg=nearest5, pch=21, col=grey(.9), cex=1.25)
legend("topright", legend=levels(gtype), fill=1:6, bty="n", cex=.75)

## A lasso penalized multinomial regression model
## running and plotting with glmnet
library(glmnet)
## we'll consider all of the content variables, interacted with RI
## You could also look at all interactions; you get diff model but similar performance OOS
xfgl <- sparse.model.matrix(type~.*RI, data=fgl)[,-1]
glassfit <- cv.glmnet(xfgl, gtype, family="multinomial")
plot(glassfit) # CV error; across top avg # nonzero across classes
## plot the 6 sets of coefficient paths for each response class
par(mfrow=c(2,3), mai=c(.6,.6,.4,.4)) ## note we can use xvar="lambda" to plot against log lambda
plot(glassfit$glm, xvar="lambda") 

## extract coefficients
B  <- coef(glassfit, select="min")
B # it's a list of coefficients, 1 matrix per glass type.
## combine into a matrix
B <- do.call(cBind, B)
## annoyingly, column names are dropped
colnames(B) <- levels(gtype) # add them back

### fit plots: plot p_yi distribution for each true yi
# use predict to get in-sample probabilities
probfgl <- predict(glassfit, xfgl, type="response")
# for some reason glmnet gives back predictions as an nxKx1 array. 
# use drop() to make it an nxK matrix
probfgl <- drop(probfgl)
# get the probs for what actually happened
# note use of a matrix to index a matrix! 
# gives back the [i,j] entry of probfgl for each row of index matrix 
# so, here, that's the probability of true class for each observation
trueclassprobs <- probfgl[cbind(1:n, gtype)] 
## plot true probs, with varwidth to have the box widths proportional to response proportion.
plot(trueclassprobs ~ gtype, col="lavender", varwidth=TRUE,
	xlab="glass type", ylab="prob( true class )") 

## classification
## looking at Head vs all others (using 0.9 rule from slides)
headclass <- probfgl[,'Head'] > .9
sum(headclass)
## you can also employ all of the binary classification idea
## whenever you are making one-vs-all-others type comparisons
## e.g., ROC plot for headlamp glass vs other glass classification
source("roc.R")
roc(p=probfgl[,'Head'], y=gtype=="Head", main="headlamp roc")

## or, straightforward max prob classification
## apply which.max prob for every row
class <- levels(gtype)[apply(probfgl,1,which.max)] 
cbind(class,as.character(gtype)) 

## Interpretation examples
## NOTE: due to CV variability your numbers may differ!
# move RI around to see what changes
RI <- 0
## For an extra 1 unit of Mg:
# @RI=0 odds of non-float over float drop by 30-40% 
exp( (B["Mg","WinNF"]+RI*B["RI:Mg","WinNF"]) -
			(B["Mg","WinF"]+RI*B["RI:Mg","WinF"]) )  
# but odds of non-float over Con increase by 60-70% 
exp( (B["Mg","WinNF"]+RI*B["RI:Mg","WinNF"]) -
			(B["Mg","Con"]+RI*B["RI:Mg","Con"]) )  

## NOTE: you can also use IC on glassfit$glm, but they don't make it easy
D = (1-glassfit$glm$dev.ratio)*glassfit$glm$nulldev
## here we are adding up the nonzero parameters across classes at each lambda
df <- 6+rowSums(sapply(glassfit$glm$beta, function(b) colSums(b!=0))) # +6 for each class type intercept
n <- nrow(xfgl)
gfAICc <- D + 2*df*n/(n-df-1)
## here, it overfits a bit (compare to OOS deviance plot)
log(glassfit$glm$lambda)[which.min(gfAICc)] 


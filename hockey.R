
library(glmnet)

load("hockey.RData")

# combine the covariates all together
x <- cbind(config, team, player) # cbind binds together two sparse matrices

# build 'y': home vs away, binary response
y <- goal$homegoal

# don't penalize config and team
factor <- c(rep(0, ncol(config)+ncol(team)), rep(1, ncol(player)))


## AICc selection 
nhlreg <- glmnet(x, y,
                 penalty.factor=factor,
                 family="binomial", standardize=FALSE)

# interesting q: why are we doing "standardize=FALSE"?

## coefficients (grab only the players)
coef.aicc <- coef(nhlreg)[colnames(player),]

#  First, a simple gut-check point: the intercept.
#  This is the effect on odds that a goal is home rather than away,
#  regardless of any info about what teams are playing or who is on ice.
#  It's the home ice advantage!  
#  We find that home-ice increases odds you've scored by 8%
exp(coef(nhlreg)[1])


## to run CV selection, just use cv.glmnet instead of glmnet
cv.nhlreg <- cv.glmnet(x, y, 
                       penalty.factor=factor,
                       family="binomial", verb=TRUE, standardize=FALSE)

## coefficients (grab only the players)
# CV selection 
coef.cv <- coef(cv.nhlreg, select="min")[colnames(player),]

#  Again, it's the home ice advantage!  
#  We find that home-ice increases odds you've scored by 8%
exp(coef(cv.nhlreg)[1])
#  Now, lets look at the player effects.
#  The regression finds about 300 significant player effects (is different every run because of CV)
sum(coef.cv!=0)
# Here are the top 10 players
coef.cv[order(coef.cv, decreasing=TRUE)[1:10]]
# Here are the bottom 10 
coef.cv[order(coef.cv)[1:10]]
#  Specifically, the model says, e.g., that whenever a goal is scored,
#  Pittsburgh's odds of having scored (rather than scored on) 
#  increase by a 42% if Sidney Crosby is on the ice.  
exp(coef.cv["SIDNEY_CROSBY"])
#  And the Blue Jackets (or Kings, pre 2011-12) odds of having scored 
#  drop by around 18% if Jack Johnson is on the ice.
exp(coef.cv["JACK_JOHNSON"])
# hockey fans among you may feel free to comment in much more detail.


## plot them together
par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(cv.nhlreg$glmnet.fit) ## cv.glmnet has included a glmnet.fit object into cv.nhlreg

log(cv.nhlreg$lambda.min)
log(cv.nhlreg$lambda.1se)

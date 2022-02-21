### European Protein Consumption, in grams/person-day ###

food <- read.csv("protein.csv", row.names=1) # 1st column is country name

## scale the data
xfood <- scale(food) 

## first, consider just Red and White meat clusters
(grpMeat <- kmeans(xfood[,c("WhiteMeat","RedMeat")], centers=3))

plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
    type="n", xlab="Red Meat", ylab="White Meat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
    col=rainbow(3)[grpMeat$cluster])

## same plot, but now with clustering on all protein groups
grpProtein <- kmeans(xfood, centers=7, nstart=50) ## change the number of centers to see what happens.
grpProtein

plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
    type="n", xlab="Red Meat", ylab="White Meat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
    col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot


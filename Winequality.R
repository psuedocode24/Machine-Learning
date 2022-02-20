#### Assignment 5###
library(dplyr)
library(ggplot2)
library(class)
library(caret)
# Read the csv file

## Question 1
## Part A
wine <- read.csv("winequality-red.csv")
wine_quality <- unique(wine$quality)
print(wine_quality)
typeof(wine_quality)
hist(wine$quality)
wine$quality_group <- dplyr::case_when(
  wine$quality == 7 | wine$quality == 8 ~ "High",
  wine$quality == 5 | wine$quality == 6 ~ "Medium",
  wine$quality == 3 | wine$quality == 4 ~ "Low"
)

# Part B
library(ggplot2)
for (i in 1:ncol(wine)){
  
   ggplot(wine, aes(x=quality_group, y=colnames(wine)[i])) + geom_boxplot()
}
## Inference:

## Fixed Acidity is more or less similar for all the quality types of wine.
## Volatile Acidity is on the higher side for low quality wine while it is very similar for the high and medium quality of wine.
## Citric Acidity is higher for the high quality for the high quality wine whereas for lowest the low quality wine.
## Residual Sugar, Chlorides, Free sulfur dioxide, pH all of them have more or less similar value for all the quality of wines.
## Total sulfur dioxide is higher for the medium quality wine while it is at the same level for both the other two quality of wine.
## Density of the high quality wine is lower than the low or medium quality wine.
## Sulfates is higher for the high quality of wine than the  other quality of wines which contain nearly the same amount of sulfates.
## The alcohol content in the high quality alcohol is more than the nearly similar alcohol content in both the low and medium quality alcohol.
## The quality of the wine is highest in the high quality wine, lowest in the low quality wine and in the middle for the medium quality wine.

# Part C

set.seed(222)

sample_size = round(nrow(wine)*.20) # setting what is 20%
index <- sample(seq_len(nrow(wine)), size = sample_size)
data_cols <- colnames(within(wine, rm(quality_group, quality)))

scaled_data <- scale(wine[,data_cols])

train <- wine[index, ]
test <- wine[-index, ]

head(train)
head(test)

x_train<- scaled_data[index,]
y_train<- wine[index ,"quality_group"]
x_test<- scaled_data[-index,]
y_test<- wine[-index, "quality_group"]

## Question 2
# Part A


seqk <- seq(10,70,5)
for (i in 1:length(seqk)) {
  k <- seqk[i]
  fit <- knn(train=x_train, test=x_test, cl=y_train, k=k)
  
  cm <- as.matrix(table(Actual = y_test, Predicted = fit))
  
  accuracy <- sum(diag(cm))/length(y_test)
  print(paste("K:", k,", Accuracy:", accuracy))
}
# We choose k = 30

# Part B

## The K Nearest neighbors determines the distance based on the nearest k observations to give the new set of observations. The k mean distance in two dimensional space 
## is the euclidean distance. Thus the distance is ranked from the closest to furthest and the kth closest distance to the furthest distance taken by taking the majority of the kth nearest dots.
## The KNN is computed using the distance of the new dot with all the previous dots. The distance is then ranked from the closest to the furthest. Further on the kth closest observations are taken, 
## the majority of the classes are counted among the k closest. the predicted class of the new observation is the majority of classes that comprised of the k nearest neighbors.
## Although being a very easy to do algorithm KNN has issue of flickering very quickly as it will flip very quickly if the value of k is changing. Thus KNN does not prove to be a stable algorithm. 
## The optimal value of K as seen from running the algorithm is the one which gives that has the highest accuracy which in this case is K = 10, with accuracy = 0.83815.

##Question 5
# Part A

## kNN (k-Nearest Neighbors) is a non-parametric classification method where we classify a certain data point based on the majority of its k-closest neighbor. 
## In kNN we assume that similar things exist in close proximity. As simple as kNN is, it is unstable as the classification changes with the number k. 
## Moreover, it only provides the classification results (and rough probability), hence we cannot readily assess misclassification risk.

## While kNN is a non-parametric model, Multinomial Logistic regression is a parametric model. 
## Multinomial Logistic Regression can be seen as an extension of binomial logistic regression, where the response is one of the k categories (i.e. rather than 0 or 1). 
## The log odds of the outcomes are modeled as a linear combination of the predictor variables (x'β). The response variable $y_i$ in this case can be written as a vector (e.g. [0,1,0,...,0]) where $y_{ik}$ = 1 if response i is class k. 
## We train the model on our training set, and evaluate the classification accuracy with our testing set.

## Contrary to the first two supervised classification approaches, clustering is unsupervised, hence we ignored the quality & quality group columns (i.e. known/labelled outcomes) above. Clustering also follows the assumption that similar data points are aggregated together. 
## In k-means, we assume that the observations are drawn from k distributions, where each distribution is normal, and we identify k number of centroids. k-means then tries to allocate clusters that minimizes sum-of-squares (total sum of squares of each element's distance to respective cluster mean). 
## The expected x is the sum of "probability of x being in cluster k multiplied by the average value of cluster k". The k-means algorithm is a trial-and-error algorithm since we start from a group of randomly selected centroids and iterate to optimize the positions of the centroids. 
## There are a few selection methods for k, such as using information criterion or the elbow method. 

## (b) Which approach would you recommend for this dataset and why?
## For this dataset, we would recommend using the multinomial logistic regression approach. Since we indeed have a known outcome (quality group), we can adopt supervised learning. 
## In addition to being faster than kNN, MLR can also produce confidence levels about its prediction, while kNN only outputs labels. In this case, we believe that using the multinomial logistic regression will allow us to classify observations more accurately.








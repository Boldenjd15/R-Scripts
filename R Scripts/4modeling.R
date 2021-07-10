library(caret)
library(dplyr)
library(rgl)
library(rpart)
library(ROCR)
library(randomForest)

set.seed(12345)

homeData <- read.table("HomeDataClean.csv", header=TRUE, row.names=1, sep=",",
                       comment.char="", colClasses=c("character", 
                       rep("factor",2), rep("numeric",4), rep("factor",3))) 

adultData <- read.table("adult.data.csv", header=TRUE, sep=",",
                       colClasses = c("numeric", "factor",
                      "numeric", "factor", "numeric", rep("factor",5),
                      rep("numeric",3), rep("factor",2)), na.strings=" ?")



load("reviewData.rda")

priceSqFtlm <- lm(Price ~ Sq..Ft., data=homeData)
summary(priceSqFtlm)


plot(homeData$Sq..Ft., homeData$Price, xlab="Square Footage", ylab="Price",
    pch=16, col="blue")
abline(priceSqFtlm, lwd=3)


predict(priceSqFtlm)


predict(priceSqFtlm, list(Sq..Ft. = c(1200,1400,2000,3500)))


priceSqFtBathdf <- data.frame(homeData[,c("Sq..Ft.","Baths","Price")])
priceSqFtBathlm <- lm(Price ~ ., data=priceSqFtBathdf)
summary(priceSqFtBathlm)


plot3d(priceSqFtBathdf)
planes3d(a=priceSqFtBathlm$coefficients[2],b=priceSqFtBathlm$coefficients[3],
         c=-1.0, d=priceSqFtBathlm$coefficients[1], alpha=0.05)


myMLRdf <- homeData[,c("Price", "Location", "Bedrooms", "Baths", "Sq..Ft.", 
                       "Realtor.Group")]
myMLRlm <- lm(Price ~ ., data=myMLRdf)
summary(myMLRlm)


myMLRsummary <- summary(myMLRlm)
myMLRpredictors <- names(which(myMLRsummary$coefficients[,4] < 0.10))
myMLRpredictors


myAdultDF <- adultData[,c("Over.Under", "education", "capital.gain")]

myAdultLR <- glm(Over.Under ~ ., data = myAdultDF, family=binomial("logit"))
summary(myAdultLR)


exp(coef(myAdultLR))


myAdultPredict <- predict(myAdultLR, myAdultDF, type="response")
myAdultPredictClass <- character(length(myAdultPredict))
myAdultPredictClass[myAdultPredict < 0.5] <- "< $50k"
myAdultPredictClass[myAdultPredict >= 0.5] <- ">= $50k"
myAdultcm <- table(myAdultDF$Over.Under, myAdultPredictClass)
myAdultcm


summary(myAdultDF$Over.Under)


myWeights <- numeric(nrow(myAdultDF))
myWeights[myAdultDF$Over.Under == " <=50K"] <- 1 
myWeights[myAdultDF$Over.Under == " >50K"]  <- 2


myAdultLR <- glm(Over.Under ~ ., data = myAdultDF, family=binomial("logit"),
                 weights=myWeights)
myAdultPredict <- predict(myAdultLR, myAdultDF, type="response")
myAdultPredictClass <- character(length(myAdultPredict))
myAdultPredictClass[myAdultPredict < 0.5] <- "<50K"
myAdultPredictClass[myAdultPredict >= 0.5] <- ">=50K"
myAdultCM <- table(myAdultDF$Over.Under, myAdultPredictClass)
myAdultCM


myAdultrpart <- rpart(Over.Under ~ ., data=myAdultDF, weights=myWeights)


plot(myAdultrpart)
text(myAdultrpart)


myAdultrpart$variable.importance


trainRows <- createDataPartition(myAdultDF$Over.Under,
				 p=0.5,
				 list=FALSE)
trainAdult <- myAdultDF[trainRows,]
testAdult <- myAdultDF[-trainRows,]


myWeights <- numeric(nrow(trainAdult))
myWeights[trainAdult$Over.Under == " <=50K"] <- 1
myWeights[trainAdult$Over.Under == " >50K"]  <- 2

myAdultrpart <- rpart(Over.Under ~ ., data=trainAdult, weights=myWeights)
myAdultPredictrpart <- predict(myAdultrpart, newdata=testAdult, type="class")
table(testAdult$Over.Under, myAdultPredictrpart)


myAdultLR <- glm(Over.Under ~ ., data = trainAdult, family=binomial("logit"),
               weights=myWeights)
adultPredictLR <- predict(myAdultLR, testAdult, type="response")
adultPredLR <- prediction(adultPredictLR, testAdult$Over.Under)
adultPerfLR <- performance(adultPredLR, "tpr", "fpr")

myAdultrpart <- rpart(Over.Under ~ ., data=trainAdult, weights=myWeights)
adultPredictrpart  <- predict(myAdultrpart, testAdult, type="prob")
adultPredrpart <- prediction(adultPredictrpart[,2], testAdult$Over.Under)
adultPerfrpart <- performance(adultPredrpart, "tpr", "fpr")


plot(adultPerfLR, col=1)
plot(adultPerfrpart, col=2, add=TRUE)
legend(0.7, 0.6, c("Log. Reg.", "Class. Tree"), col=1:2, lwd=3)


performance(adultPredLR, "auc")
performance(adultPredrpart, "auc")



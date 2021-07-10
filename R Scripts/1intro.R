myLibraries <- c("knitr", "magrittr", "markdown", "dplyr", "ggplot2", "rpart", "nnet",
                 "caret", "rmarkdown", "Rcpp", "cluster", "NeuralNetTools",
                 "randomForest", "kernlab", "e1071", "dummies", "rgl", "reshape2",
                 "fpc", "ROCR")
install.packages(myLibraries)

lapply(myLibraries, library, character.only=TRUE)

3+3
3*3
3/3
3^3

sqrt(3)

(a <- c(1:9))

(b <- c(1,3,8,12))

(mylist <- rep("R is so cool", 5))

(A <- matrix(a,ncol=3,byrow=TRUE))

?matrix


mydata <- data.frame(A)
names(mydata) <- c("Col1", "Col2", "Col3")
mydata

myfunction <- function(x) {
  x <- x + 1
  x
}
(myresult <- myfunction(a))


senicData <- read.table("SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),rep("numeric",9),
                                     rep("factor",2)))


head(senicData)


senicData <- senicData[,-ncol(senicData)]


head(senicData)


adultData <- read.table("adult.data.csv", header=TRUE, sep=",",
                       colClasses = c("numeric", "factor",
                      "numeric", "factor", "numeric", rep("factor",5),
                      rep("numeric",3), rep("factor",2)), na.strings=" ?")


head(adultData)


write.csv(adultData, file="adultDataClean.csv", row.names=FALSE)


read.table("adultDataClean.csv", header=TRUE, sep=",",
                        colClasses = c("numeric", "factor",
                       "numeric", "factor", "numeric", rep("factor",5),
                       rep("numeric",3), rep("factor",2)))


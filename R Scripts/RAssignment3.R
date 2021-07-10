######################################################################################################################
#Group 4, R Assignment #3
#Jeremy Bolden, Saheli Das, Joe Gardner, Nick Roy, Qiyenda Wilson, Reeves Garnett 
#Due Date: 12/10/19
#
#Question 1: Create a tfidf matrix for the reviews about microwaves.
#
######################################################################################################################
#We start pulling the csv file into the environment

install.packages("tidytext", "tm", "tokenizer")
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tokenizers)
library(tm)
library(dummies)
library(rgl)
library(cluster)
     #Establish libraries we will be using

reviewData <- read.csv("Reviews for VCU.csv", sep=",")  #Call in the Review Data

#We can use the summary() command to view our data set and take a look at entire data set
summary(reviewData)
#Or to view just the column names we can use:
colnames(reviewData)
colnames(reviewData)[1] <- "Date"                     #changes the label of the first column
colnames(reviewData)                                  #displays the column names again
save(reviewData, file="reviewData.rda")               #overrittes the saved filed for the name change

reviewData$Date <- as.POSIXct(reviewData$Date, origin = "1970-01-01") #converts into R standard time

######################################################################################################################

microwaveData <- reviewData %>%
  filter(Family == 'MICROWAVES') %>%
  distinct(Date, Review.ID, SKU, Review_Text, Rating, Review.Site)

#Changes review text to character from factor
microwaveData$Review_Text <- as.character(microwaveData$Review_Text)

#Now we need to filter the text, eliminating caps, spcial characters and numbers/symbols
microwaveData$Review_TextFiltered <- tolower(microwaveData$Review_Text)    #creates new column with no caps letters
microwaveData$Review_TextFiltered <- gsub("[][!#$%()&*,.:;<=>@^_`|~.{}\"]", "", microwaveData$Review_TextFiltered) #removes special characters
microwaveData$Review_TextFiltered <- gsub('[0-9]+', '', microwaveData$Review_TextFiltered) #removes numbers
microwaveData$Review_TextFiltered <- gsub("[^0-9A-Za-z///' ]","" , microwaveData$Review_TextFiltered,ignore.case = TRUE) #more special characters removed

#Now to create the TF-IDF matrix function...
tf_idf_func <- function(vector) 
{
  corpus  <- Corpus(VectorSource(vector))
  root_list <- list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE)

  tf <- TermDocumentMatrix(corpus, control=root_list) %>% as.matrix()
  idf <- (log(ncol(tf)/(rowSums(tf)))) %>% diag()
  tf <- tf/colSums(tf)
  
  tfidf <- crossprod(tf, idf)
  colnames(tfidf) <- rownames(tf)
  
  tfidf <- tfidf/sqrt(rowSums(tfidf^2))
  return (tfidf)
}



#Using the tf_idf_func created, we can now filter through all of our text data and create the matrix
reviewtfidf <- tf_idf_func(microwaveData$Review_TextFiltered)
print(reviewtfidf)
                                       #Confirms no strange strings in the text column

######################################################################################################################
#
#Problem #2: Cluster the reviews into three groups.
#
######################################################################################################################

library(dummies)
library(rgl)
library(cluster)        #calls the libraries we will be using if you haven't already done so

reviewkmeans <- kmeans(reviewtfidf, centers=3) #using kmeans from class we seperate into 3 groups by cluster

summary(reviewkmeans)

######################################################################################################################
#
#Problem #3: Use principal component analysis to identify characteristics of each cluster.  
#            Which words are most associated with the first two principal components?
#
######################################################################################################################
#using the code from class
reviewPCA <- prcomp(reviewtfidf, retx=TRUE)


#plot the data 
plot(reviewPCA$x[,1:2], col=reviewkmeans$cluster, pch=reviewkmeans$cluster)
plot3d(reviewPCA$x[,1:3], col = reviewkmeans$cluster)


summary(reviewPCA)


pca.1 <- as.data.frame(as.table(reviewPCA$rotation[,1])) #need to add as.table to include the text column else the next part wont work!!!
pca.2 <- as.data.frame(as.table(reviewPCA$rotation[,2]))


# Now we can find the top 10 best and worst words 
pca.1 %>% top_n(-10) %>% 
  arrange(desc(Freq)) #best terms
pca.1 %>% top_n(10) %>% 
  arrange(desc(Freq)) #worst terms



# print top 10 best and worst terms for PCA.2
pca.2 %>% top_n(-10) %>% 
  arrange(desc(Freq))
pca.2 %>% top_n(10) %>% 
  arrange(desc(Freq))



#We can also look at a direct comparison of the freq for each word alphabetically too
head(pca.1)
tail(pca.1)

head(pca.2)
tail(pca.2)

######################################################################################################################
#
#End of Assignment R Assignment for Group 4. 
#
######################################################################################################################
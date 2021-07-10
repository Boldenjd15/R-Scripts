#########################################################################################################
#Group 4, R Assignment #1 
#Jeremy Bolden, Saheli Das, Joe Gardner, Nick Roy, Qiyenda Wilson, Reeves Garnett 
#Due Date: 10/25/2019
#
#Question 1: How many families of products are in the dataset (Hamilton Beach Review Data)?
#
#########################################################################################################
#We start pulling the csv file into the environment
library(dplyr)                                             #Establish libraries we will be using

reviewData <- read.csv("Reviews for VCU.csv", sep=",")     #Reads csv file saved in the Directory

#########################################################################################################
#We can use the summary() command to view our data set and take a look at entire data set
summary(reviewData)
#Or to view just the column names we can use:
colnames(reviewData)
#########################################################################################################
#For some reason the Data column was misslabeld so to change that for the future we need 
#to write the following:
#########################################################################################################
reviewData[,6] 
colnames(reviewData)[1] <- "Date"                     #changes the label of the first column
colnames(reviewData)                                  #displays the column names again
save(reviewData, file="reviewData.rda")               #overrittes the saved filed for the name change

#########################################################################################################
#To count the number of families of products there are, we simply use the length() command
#and state the data set and which Column we are looking for
#########################################################################################################
summary(reviewData$Family)                           #Returns number of products per family

Num_Families <- length(summary(reviewData$Family))   #counts and creates variable for total families 
length(levels(reviewData[,"Family"]))                #another method used to confirm formula
print(Num_Families)                                  #Total number of families = 52

#########################################################################################################
#
#
#Question #2 for R assignment 1: What is the average time between reviews of toasters?
#
#
#########################################################################################################
#The code below pulls all of the columns from reviewData based on the condition Toasters
#the comma at the end shows that we want all of the columns
#########################################################################################################
toasterData <- reviewData[reviewData[,4] == "TOASTERS",] 

#To find the average time between toaster reviews we first confirm Date is classified correctly

dateData <- toasterData[,1] ## Pulls the columns of dates into a single set of values
dateData <- 'class<-'(dateData, "num")  #Reclassifies dateDate into Date" level for dplyr function later
                                        #when proofing the coding/math
#########################################################################################################
#Once we convert to a Date num level, need can use the dplyr function to arrange and group
#by Toasters specifically as well as the date
#Now we want to narrow down the information even more and select the Date for all Toasters
#########################################################################################################

dateDataSubset <- toasterData  %>%
  dplyr:: select(Date,Family) #Returns Family column as well to confirm only Toaster reviews 
  

summary(dateDataSubset)  #Shows us number of reviews by date for just toasters, 
                         #Not necessary but interesting to see numbers of reviews for top 5 days

#########################################################################################################
Date_Count = length(levels(dateDataSubset[,1])) #creates variable Date_Count for total # of days for toaster reviews

Review_Count = length(dateDataSubset[,1])       #counts total number of reviews for Toasters

Avg_Reviews_Per_Day = (Review_Count/Date_Count) #Calculates number of reviews/day
print(Avg_Reviews_Per_Day)                      #Prints answer = 27.72051 Toaster reviews/day

Avg_Time_Per_Review = (Date_Count/Review_Count) #Calculates average time of reviews for Toasters
print(Avg_Time_Per_Review)                      #Prints answer = 0.03036 days/review

Avg_Time_Per_Review <- Avg_Time_Per_Review * 24 #Converts avg time to Hours = 0.8658 Hours/review

Avg_Time_Per_Review <- Avg_Time_Per_Review * 60 #Converts avg time to Minutes = ~52 minutes/review
print(Avg_Time_Per_Review)                      #Prings answer = 51.947 minutes/review ~ 52min
#########################################################################################################
#If we want to find the average difference in date, we can use the dplyr function shortcut too
dateDataSubset %>%        #identifies which data set we are pulling from (derived above)
    summarise(avg = as.numeric(mean(diff(dateData))))  #calculates average time between dates =0.03036

#From this we can prove our lengthy method as the above dplyr function give the same average time
# between reviews. We can then convert it down to hours and minutes if needed using the conversions
#########################################################################################################
#
#
#Question 3 for R assignment 1: How many unique toaster models are reviewed in the dataset?
#
#########################################################################################################
#Because we already have the toasterData, we can again use the LENGTH and UNIQUE operators to 
#calculate the number of unique models of toasters based upon the SKUs
total_SKUs <- length(toasterData[,"SKU"])   #Counts number of SKUs from toasterData = 2992

unique_models <- unique(toasterData$SKU)            #Identifies the number of unique SKUs and removes duplicates

print(length(unique_models))                        #Prints number of SKUs = 166 different models

#########################################################################################################
#
#
#Question 4 for R assignment 1: Create a table of the number of reviews by month.
#
#
#########################################################################################################
class(dateDataSubset$Date)                      #Prints the class type of column Date

dateDataSubset$Date <- as.POSIXct(dateDataSubset$Date, origin = "1970-01-01") #converts into R standard time

hist(dateDataSubset$Date, "month")              #generates a cool histogram of reviews by month 
                                                #will be using this for the Hamilton Beach Case!!! 


library(lubridate)                              #Call the Library utilized for date functions
reviews.by.month <- month(dateDataSubset$Date)  #Creates variable based on month conversion
mytable <- table(reviews.by.month)              #Creates the table from Toaster reviews by month calculated above

months <- list("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
                                                #^^^ Creates a list of abbreviated month names for col headers
names(mytable) <- months                        #Assigns the list of strings to the col headers in the table
print(mytable)       


totalreviewsbymonth <- month(reviewData$Date)   #Same process but for total reviews for all products by month
mytotaltable <- table(totalreviewsbymonth)      #Creates table based on total reviews/month
names(mytotaltable) <- months                   #Assigns previously generated month col names above
print(mytotaltable)

sum(mytotaltable)                               #Confirms we have a count of total number of reviews

#########################################################################################################
#                                                                                                       #
#End of R Assingment #1 for Group #4.                                                                   #
#                                                                                                       #
#########################################################################################################















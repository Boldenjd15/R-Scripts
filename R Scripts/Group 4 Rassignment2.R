######################################################################################################################
#Group 4, R Assignment #2 
#Jeremy Bolden, Saheli Das, Joe Gardner, Nick Roy, Qiyenda Wilson, Reeves Garnett 
#Due Date: 11/22/2019
#
#Question 1: Plot a histogram of number of reviews per day.  
#            Is the distribution skewed or approximately normal?
#
######################################################################################################################
#We start pulling the csv file into the environment

library(dplyr)                                             #Establish libraries we will be using

reviewData <- read.csv("Reviews for VCU.csv", sep=",")  

######################################################################################################################
#We can use the summary() command to view our data set and take a look at entire data set
summary(reviewData)
#Or to view just the column names we can use:
colnames(reviewData)
colnames(reviewData)[1] <- "Date"                     #changes the label of the first column
colnames(reviewData)                                  #displays the column names again
save(reviewData, file="reviewData.rda")               #overrittes the saved filed for the name change

reviewData$Date <- as.POSIXct(reviewData$Date, origin = "1970-01-01") #converts into R standard time
hist(reviewData$Date, "day")  
#We can see that the total histogram is skewed to the left given all reviews. This is mainle due to the fact that
#there are more overall reveiws as time goes on. There are a few peaks of times for reviews because of seasonality
#however the average number of reviews are to the right.
######################################################################################################################
reviewsPerDay <- reviewData %>%
  dplyr::select(Date) %>%
  dplyr::mutate(Date = format(Date,"%y-%m-%d")) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(numReviews=dplyr::n())             #Selects column of date, changes format to y/m/d and displays 
                                                      #second column of the number of reviews for that day

hist(reviewsPerDay$numReviews)                        #Displays a histogram of the number of reviews per day
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 500])
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 1000])
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 2000])
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 3000])
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 4000])
hist(reviewsPerDay$numReviews[reviewsPerDay$numReviews < 5000], xlab="Reviews", main="Histogram of Reviews Per Day")
#Allows us to see a bit better picture of reviews per day

#From this histogram, we can see the number of reviews per day are skewed to the right when filtering by days with less than 5k reviews

######################################################################################################################
#
#Question #2: Create a boxplot of the number of reviews by the day of week.  Do there appear to be differences by day?
#
######################################################################################################################
reviewsbyDayOfWeek<- reviewData %>%
  dplyr::select(Date) %>%
  dplyr::mutate(date = as.POSIXct(format(Date, "%y-%m-%d"))) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(numReviews=dplyr::n()) %>%
  dplyr::mutate(day = format(date, "%a"))           #Creates a variable for reviews by day of the week  


boxplot(reviewsbyDayOfWeek$numReviews ~ as.factor(reviewsbyDayOfWeek$day))  #creates a boxplot of reviews by day of week 
                                                                            #must be in factor form to populate dates

#From this it's hard to read the box plots accurately so we can elimate the outlier days for reviews >5k for a better view


dayofweeksubset <- reviewsbyDayOfWeek[reviewsbyDayOfWeek$numReviews < 5000,] #creates a subset of data restricting less than 5k
boxplot(dayofweeksubset$numReviews ~ as.factor(dayofweeksubset$day))         #returns the boxplots based on new subset


#From the list of boxplots for the days of the week, we can see that Friday appears to have the greatest amount of variation
#in the number of reviews. We can also see that the average number of reviews per day of the week is similar, floating around
#the 1k mark. 

######################################################################################################################
#
#Question #3: Create a line plot with the number of reviews per month, with one line for each review site. 
#Of the top three sites(in terms of number of reviews), which is the least sensitive to seasonality? 
#
######################################################################################################################
library(ggplot2)                                #Calls the library used to plot the lines on a graph

reviewsByMonthPerSite<- reviewData %>%
  dplyr::select(Date, Rating, Review.Site) %>%
  dplyr::mutate(month = as.POSIXct(format(Date, "%y-%m-%d"))) %>%
  dplyr::group_by(month, Review.Site) %>%
  dplyr::summarise(MonthReviews=n())            #creates 3 columns of date, rating and review site in R standard time
                                                #and groups them by month and review site. 

ggplot(reviewsByMonthPerSite, aes(x=month, y = MonthReviews, group = Review.Site, color = Review.Site)) + geom_line()
#Using ggplot, we pull from the calculated data above and sort by group and graph each review site by seperate color
#Again it's difficult to see the whole picture here as Amazon, Target and Walmart dominate the dataset. 

monthsitesubset <- reviewsByMonthPerSite[reviewsByMonthPerSite$MonthReviews < 2000,] #creates subset with restriction
                                                                                     #of 2000 reviews by month

ggplot(monthsitesubset, aes(x=month, y = MonthReviews, group = Review.Site, color = Review.Site)) + geom_line()
#This is somewhat better but we want to look at the top 3 sites seperate so let's break them down into separate plots

#Creates a subset for all info based on review site      (Start here to change threshold in next line)
Walmartsubset <- reviewsByMonthPerSite[reviewsByMonthPerSite$Review.Site == "Walmart",]
Targetsubset <- reviewsByMonthPerSite[reviewsByMonthPerSite$Review.Site == "Target",]
Amazonsubset <- reviewsByMonthPerSite[reviewsByMonthPerSite$Review.Site == "Amazon",]

#Further filters the data by number of MonthReviews less than 10k
Walmartsubset <- Walmartsubset[Walmartsubset$MonthReviews < 10000,]
Targetsubset <- Targetsubset[Targetsubset$MonthReviews < 10000,]
Amazonsubset <- Amazonsubset[Amazonsubset$MonthReviews < 10000,]

#Creates the Plots for the three sites we are looking for
WalmartPlot <- ggplot(Walmartsubset, aes(x=month, y = MonthReviews, group = Review.Site, color = Review.Site,)) + geom_line()
TargetPlot <- ggplot(Targetsubset, aes(x=month, y = MonthReviews, group = Review.Site, color = Review.Site)) + geom_line()
AmazonPlot <- ggplot(Amazonsubset, aes(x=month, y = MonthReviews, group = Review.Site, color = Review.Site)) + geom_line()

#Hit Contol+Enter to see each plot seperate.
print(WalmartPlot + labs(title= "Reviews for Walmart by Month",y="Monthly Reviews", x = "Month"))
print(TargetPlot + labs(title= "Reviews for Target by Month", y="Monthly Reviews", x = "Month"))
print(AmazonPlot + labs(title= "Reviews for Amazon by Month", y="Monthly Reviews", x = "Month"))


#Looking at the data for Amazon, Walmart and Target we can see that Amazon appears to be the least affected by seasonality 
#with a steady increase over time. The Walmart and Target plots show seven distinct peaks at the beginning/end of each year 
#showing that the primary time for reviews placed on those site is around the holidays and not as much activity thoughout 
#the rest of the year. 


help(ggplot)
######################################################################################################################
#
#End of Assignment R Assignment for Group 4. 
#
######################################################################################################################
  






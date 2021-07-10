library(dplyr)
library(reshape2)

adultData <- read.table("adult.data.csv", header=TRUE, sep=",",
                       colClasses = c("numeric", "factor",
                      "numeric", "factor", "numeric", rep("factor",5),
                      rep("numeric",3), rep("factor",2)), na.strings=" ?")

load("reviewData.rda")

adultData[2,3]

adultData$occupation
adultData[,7]
adultData[,c("occupation")]
adultData[,grep("occupation", names(adultData))]

adultData %>% dplyr::select(occupation)
adultData %>% dplyr::select(7)

capitalGainsK <- adultData$capital.gain/1000
capitalGainsK[1:100]


adultData[,c(1,7)]
adultData[,c("Age", "occupation")]
adultData %>%  dplyr::select(Age, occupation)
adultData %>% dplyr::select(1, 7)


mastersData <- adultData[adultData[,4] == " Masters",]


mastersData <- filter(adultData, education == " Masters")


summary(adultData$education)
adultData$education <- factor(adultData$education, levels =c(
  " Preschool",  " 1st-4th",  " 5th-6th",  " 7th-8th",  " 9th",  " 10th",  " 11th", 
  " 12th",  " HS-grad",  " Some-college", " Assoc-voc", " Assoc-acdm",
  " Bachelors", " Masters", " Prof-school", " Doctorate"))


head(adultData[order(adultData$hours.wk, adultData$capital.gain, decreasing=TRUE),])


head(arrange(adultData, desc(hours.wk), desc(capital.gain)))


dim(adultData)
nrow(adultData)
ncol(adultData)


summary(adultData$education)


(mysummary <- summary(adultData))


occupationMissing <- is.na(adultData$occupation)
occupationMissing[1:10]


apply(is.na(adultData),2,sum)


sum(is.na(adultData))


table(adultData$native.country, adultData$sex)


(avgCapitalGains <- adultData %>%
                      group_by(education, marital.status) %>%
                      dplyr::select(education, marital.status, capital.gain) %>%
                      summarise(mean(capital.gain)))


(avgCapitalGainsTable <- dcast(avgCapitalGains,education ~ marital.status, 
                              value.var="mean(capital.gain)"))


(myMeltedTable <- melt(avgCapitalGainsTable, id.vars = "education", 
                      measure.vars=c(" Divorced"," Married-AF-spouse",
                                     " Married-civ-spouse"," Married-spouse-absent", 
                                     " Never-married", " Separated", " Widowed")))


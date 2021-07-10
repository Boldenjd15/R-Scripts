library(ggplot2)
library(reshape2)
library(rgl)
library(dplyr)


senicData <- read.table("SENIC.csv", header=TRUE, row.names=1, sep=",",
                        colClasses=c(rep("numeric",7),rep("factor",2),
                        rep("numeric",9), rep("factor",2)))
senicData <- senicData[,-ncol(senicData)]


load("reviewData.rda")


hist(senicData$Culture_ratio)


hist(senicData$Culture_ratio, col="red",border="blue", 
     xlab="Culture Ratio", main="Histogram of Culture Ratio") 


dev.off()


pdf("cultureHist.pdf")
hist(senicData$Culture_ratio, col="red", border="blue", 
     xlab="Culture Ratio", main="Histogram of Culture Ratio") 
dev.off()


png("cultureHist.png")
hist(senicData$Culture_ratio, col="red", border="blue", 
     xlab="Culture Ratio", main="Histogram of Culture Ratio") 
dev.off()


logCultureRatio <- log(senicData$Culture_ratio)
hist(logCultureRatio)


normalizedLogCultureRatio <- scale(logCultureRatio)
hist(normalizedLogCultureRatio)


ggplot(senicData, aes(x=Culture_ratio)) + 
    geom_histogram()


regionCounts <- table(senicData$Region_Name)
barplot(regionCounts)


boxplot(senicData$Length_stay ~ senicData$Region_Name)


medschoolRegionTable <- table(senicData$Region_Name, senicData$Medical_School)
mosaicplot(medschoolRegionTable, color=c(1:2), xlab="Region",
           ylab="Medical School",main="")


plot(x=senicData$Age_years, y=senicData$Length_stay, 
     xlab="Average Age", ylab="Average Length of Stay", 
     main="Plot of Average Length of Stay versus Average Age",
     col=as.numeric(senicData$Medical_School),
     pch=as.numeric(senicData$Medical_School))
legend("topleft", title="Med School Affiliation?", legend=c("No", "Yes"), 
       col=1:nlevels(senicData$Medical_School),
       pch=1:nlevels(senicData$Medical_School)) 


plot3d(senicData$Length_stay, senicData$Age_years, senicData$Infection_pct, 
       col=as.numeric(senicData$Region_Name), type="s", 
       xlab="Average Length of Stay",ylab="Average Age", 
       zlab="Average Prob. of Infection")
legend3d("topright", levels(senicData$Region_Name), 
         col=1:nlevels(senicData$Region_Name), pch=16)



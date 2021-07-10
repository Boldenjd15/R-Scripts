######################################################################################################################
#Group 4, Stats 2 Assignment #3
#Jeremy Bolden, Saheli Das, Joe Gardner, Nick Roy, Qiyenda Wilson, Reeves Garnett 
#Due Date: 2/23/2020
#
#Question 1d: Provide individual and/or variable representation on the canonical variates 
#(i.e., use plt.cc in R). Interpret what you see.
#
######################################################################################################################

#For this HBAT data se, we replaced all of the "Excellent" comments with 10 to make the column consistently numeric

#Call the libraries we will be using. 
library(CCA)
library(ggplot2)
library(GGally)

#import HBAT Data
HBAT <- read.table("HBAT_200.txt",sep=",",header=TRUE) 

#Create our variables from columns X6-X18 and X19-X22
Y <- as.matrix(cbind(HBAT[,20:23]))
X <- as.matrix(cbind(HBAT[,7:19]))

#plot bivariate correlations
correl <- matcor(X,Y)
img.matcor(correl,type=2)
ggpairs(HBAT)

#Canonical Correlation Analysis 
ex.cca <- cc(X,Y)
barplot(ex.cca$cor, xlab="Dimension", ylab="Canonical Correlations")
plt.cc(ex.cca, var.label=TRUE)

######################################################################################################################
#
#End of R Code for Question 1d 
#
######################################################################################################################


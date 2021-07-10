Candydata <- read.csv('C:/Users/bolde/Desktop/School Work Spring 2020/Statistics 2/CandyBars.txt', sep=',',header=TRUE)






x <- as.matrix(Candydata)
center <- colMeans(x) 
n <- nrow(x);
p <- ncol(x);
cov <- cov(x);
d <- mahalanobis(x,center, cov)
qqplot(qchisq(ppoints(n),df=p),d, main = "QQ Plot", ylab = "Mahalnobis")
abline(a=0, b=1)

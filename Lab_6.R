# Lab 6: SAN ----
set.seed(12345)
fiveone <-  rep(1,5)
out <- c()
for (x in 1:1000) {
out <- c(out,SAN(fiveone))
}
mean(out)

int <- c(mean(out) - qnorm(0.975)*sqrt(var(out))/sqrt(1000),
         mean(out) + qnorm(0.975)*sqrt(var(out))/sqrt(1000))
int

# lab 6: IU ----
# Q1
SANData <- read.csv("SAnData.csv")
library(tidyverse)
SANData %>% glimpse()
set.seed(12345)
IU(SANData,n=30,b=100)
# Q2
set.seed(12345)
MySANData <- cbind(rexp(1000, rate=1/10), rexp(1000, rate=1/15),
                   rexp(1000, rate=1/10), rexp(1000, rate=1/10),
                   rexp(1000, rate=1/15))

# set b=10 (number of bootstrap samples), vary datasize
IU(MySANData[1:10,],n=10,b=100)
IU(MySANData[1:100,],n=10,b=100)
IU(MySANData[1:1000,],n=10,b=100)
# set b=100
IU(MySANData[1:10,],n=10,b=100)
IU(MySANData[1:100,],n=10,b=100)
IU(MySANData[1:1000,],n=10,b=100)

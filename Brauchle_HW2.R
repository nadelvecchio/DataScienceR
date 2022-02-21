library(dplyr)

Auto <- read.csv("Auto.csv", 
                 stringsAsFactors = FALSE, 
                 na.strings = "?") 

#8a i - iii
auto_slm <- lm(mpg ~ horsepower, data = Auto)
summary(auto_slm)

#8aiv
predict (auto_slm ,data.frame(horsepower = 98), interval="confidence")
predict (auto_slm ,data.frame(horsepower = 98), interval="prediction")

#8b
plot(Auto$horsepower, Auto$mpg) 
abline(auto_slm, lwd = 2, col = "blue")

#8c
par(mfrow=c(2,2))
plot(auto_slm)


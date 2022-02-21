library(dplyr)
library(ggplot2)
library(RColorBrewer)
# create euclidean distance dataframe
euc <- data.frame("X1" = c(0,2,0,0,-1,1), "X2" = c(3, 0, 1, 1, 0, 1), "X3" = c(0,0,3,2,1,1), "Y" = c("red", "red", "red", "green", "green", "red"))
#calculate the distance
euc_dist <- euc %>% mutate(distance = sqrt((X1-0)**2 + (X2-0)**2 + (X3-0)**2))
euc_dist


college <- College
rownames(college) = college[,1]
fix(college)
college = college[,-1]

# check num of private/non-private
table(college$Private)

# summarize college dataset
summary(college)

#scatterplots
pairs(college[,1:10])


#histograms
par(mfrow=c(2,2)) 
hist(college$Grad.Rate, main = "Graduation Rate")
hist(college$Expend, main = "Expenditure/Student")
hist(college$PhD, main = "# Faculty PhDs")
hist(college$Outstate, main = "Tuition Out-of-State")

# define elite variable
Elite=rep("No",nrow(college )) 
Elite[college$Top10perc >50]=" Yes" 
Elite=as.factor(Elite) 
college=data.frame(college ,Elite)

summary(college$Elite)
plot(college$Elite, college$Outstate)

# calculate percent accepted
college = college %>% mutate(perAccepted = Accept/Apps)

# create grid graphics

p1 <- college %>% ggplot(aes(perAccepted, fill = Elite)) + geom_histogram() +  scale_fill_brewer(palette = "Paired")
p1
p2 <- college %>% ggplot(aes(Private, PhD, fill = Elite)) + geom_boxplot() +  scale_fill_brewer(palette = "Paired")
p3 <- college %>% ggplot(aes(perAccepted, Outstate, color = Elite)) + geom_point() +  scale_color_brewer(palette = "Paired")
p4 <- college %>% ggplot(aes(Grad.Rate, fill = Elite)) + geom_histogram() +  scale_fill_brewer(palette = "Paired")
grid.arrange(p1, p2, p3, p4)


### BOSTON ###

library(MASS)
Boston
summary(Boston)
#scatter
pairs(Boston)

# create grid of Boston visualizations
par(mfrow=c(2,2))
plot(Boston$nox, Boston$age, xlab = "Nitrogen oxides concentration", ylab = "Proportion of homes built before 1940")
plot(Boston$rm, Boston$medv, xlab = "Average # of rooms", ylab = "Median value of homes" )
plot(Boston$rm, Boston$lstat, xlab = "Average # of rooms", ylab = "Percent lower status of population")
plot(Boston$medv, Boston$lstat, xlab = "Median value of homes", ylab  = "Percent lower status of population")


# 
par(mfrow=c(2,2))
plot(Boston$crim, Boston$age, xlab = "Per capita crime rate", ylab = "Proportion of homes built before 1940")
plot(Boston$crim, Boston$dis, xlab = "Per capita crime rate", ylab = "Weighted mean distances of ifve Boston employment centers")
plot(Boston$crim, Boston$ptratio, xlab = "Per capita crime rate", ylab = "Pupil-teacher ratio")
plot(Boston$crim, Boston$medv, xlab = "Per capita crime rate", ylab = "Median value of homes")

# find min and max of these variables
Boston %>% dplyr::select(crim, tax, ptratio) %>% summarize_all(max)
Boston %>% dplyr::select(crim, tax, ptratio) %>% summarize_all(min)

# find all the means
Boston %>% summarize_all(mean)

# find mean of places with more than an avg of8 rooms per dwelling
Boston %>% filter(rm > 8) %>% summarize_all(mean)

# find number of suburbs next to river
Boston %>% filter(chas == 1) %>% summarize(count= n())

# median pupil to teacher ratio
median(Boston$ptratio)

# find out m ore about suburb with lowest home values
Boston %>% slice(which.min(medv))




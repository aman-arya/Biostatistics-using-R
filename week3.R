######################################################
#Biostatistics course in R - Week 3
######################################################

#groups for assignment-1.

#Do you need to formulate hypothesis?
#You can state your hypothesis and explain your reasoning.
#Providing the code and your detailed comments is required.

#What should i do in R to answer the assignment?
#You can explore the dataset based on what you have learnt in R.
#But the goal is for you to "learn to learn" from R documentation.
#Use of new functions, packages is encouraged. 

#What kind of a pattern should you find?
#Try to think of the pattern as something that will explain a major world event.
#A pattern that is consistent with world events is not surprising.
#Can you find a pattern which is unexpected but upon introspection makes sense?

#comparing life expectancy between countries

boxplot(SwedenLife,IndiaLife,names=c("Sweden","India"),
        ylab="Life Expectancy", xlab="Countries")

boxplot(SwedenLife,IndiaLife,BanglaLife, names = c("Sweden","India","Bangladesh"))

data.frame(lifeExpect=c(SwedenLife,IndiaLife,BanglaLife)
           ,Country=c(rep("Sweden",length(SwedenLife)),rep("India",length(IndiaLife)),
                      rep("Bangladesh",length(BanglaLife))))->CountryBox

boxplot(CountryBox$lifeExpect~CountryBox$Country)

#A t-test is a parametric test - certain assumptions need to be met. Normal dist
#A t-test is not a non-parametric test

#single sample t test ----
single_sample <- rnorm(10,mean=10) # using a mean of 10
t.test(single_sample, mu = 10)		#test if average=10

single_sample = rnorm(10,mean=100)# using a mean of 100
t.test(single_sample, mu = 10)		#test if average=10

t.test(IndiaLife,mu=50) # this will not work --- assumption not met


#Two sample t-test ----

x = rnorm(10)
y = rnorm(10)
boxplot(x,y,names=c("X","Y"))

t.test(x,y)

x = rnorm(10)
y = rnorm(40)
boxplot(x,y,names=c("X","Y"))
boxplot(x,y,names=c("X","Y"),outline=F)

t.test(x,y)

#Paired
t.test(x,y,paired=T)

t.test(x,y)
#number of observations don't match. So cannot be a paired data set.
t.test(x,y,paired=T)

#can we use the t-test on life expectancy data?
t.test(SwedenLife,IndiaLife)
t.test(SwedenLife,IndiaLife, var.equal = TRUE)
t.test(SwedenLife,IndiaLife, var.equal = FALSE)

t.test(SwedenLife,IndiaLife, var.equal = TRUE, alternative = "greater")
t.test(SwedenLife,IndiaLife, var.equal = TRUE, alternative = "less")

t.test(SwedenLife,IndiaLife, paired=TRUE)
#exact argument can be set to FALSE, will not try to calculate exact p-value
t.test(SwedenLife,IndiaLife, paired=TRUE,exact=F)

#using a non-parametric alternative
wilcox.test(SwedenLife,IndiaLife, paired=TRUE) 

wilcox.test(SwedenLife,IndiaLife, paired=TRUE,alternative="greater") 

wilcox.test(SwedenLife,IndiaLife, paired=TRUE,alternative="less") 

na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Bangladesh",])))->BanglaLife

par(mfrow=c(1,2))
hist(BanglaLife)
hist(IndiaLife)
boxplot(BanglaLife,IndiaLife,names=c("Bangladesh","India"))

wilcox.test(BanglaLife,IndiaLife, paired=TRUE) 
# what if the data was not from the same years?
wilcox.test(BanglaLife,IndiaLife, paired=FALSE)

wilcox.test(BanglaLife,IndiaLife, paired=TRUE, alternative = "greater")
wilcox.test(IndiaLife,BanglaLife, paired=TRUE, alternative = "greater")


summary(BanglaLife)
summary(IndiaLife)
length(lifeExpectancyInYears[2,])
#Installing packages and using them ----

install.packages("gapminder")
#load the installed package
library(gapminder)
str(gapminder_unfiltered)
str(gapminder)

summary(gapminder_unfiltered)

install.packages("Hmisc")
#load the installed package
library(Hmisc)

#attaching a dataframe to the R path ----

setwd("E:/Courses/Biostatistics/GapMinderData")#But this will work 

lifeExpectancyInYears <- read.csv("life_expectancy_years.csv", header = T, check.names = FALSE) 

lifeExpectancyInYears$country # will show this vector
country #just typing country will not show the vector

attach(lifeExpectancyInYears)
country #just typing country will now show the vector as its attached
#detach reverses the attach function
detach(lifeExpectancyInYears)

#Exploratory Data Analysis or EDA ----

describe(gapminder_unfiltered)

dim(gapminder)# dimensions of an object. rows and columns of tibble
nrow(gapminder)# number of rows
ncol(gapminder)# number of columns
names(gapminder)# here it shows colnames
colnames(gapminder)#
rownames(gapminder)#
summary(gapminder)# how is it different from describe?

gm.subset <- gapminder[gapminder$year %in% c(2002, 2007),]
#perform a function for each country
aggregate(gm.subset$gdpPercap, by = list(gm.subset$country), FUN = mean)

aggregate(gm.subset$gdpPercap, by = list(gm.subset$country
                                         ,gm.subset$year), FUN = mean)

#using sapply(), tapply(), and apply()
#check the data type of every column 
sapply(gapminder, class)

#perform function on each column
apply(gapminder, 2, class)
#only for numeric columns
apply(gapminder[,5:6], 2, mean)
#perform function on each row
apply(gapminder[,5:6], 1, max)
#tapply works similar to aggregate - but output format differs
tapply(gm.subset$gdpPercap, gm.subset$country, FUN = mean)
str(tapply(gm.subset$gdpPercap, gm.subset$country, FUN = mean))

#subset based on conditions
gapminder[gapminder$lifeExp < 30, 'country']
#subset using the subset function
subset(gapminder, lifeExp < 30, select = 'country')
######################################################
#Biostatistics course in R - Week 2
######################################################
#You can check the data type of a using keyword class().
num <- 1.2
print(num)
class(num)

#get random numbers from a Normal distribution with mean 25 and sd of 4
rnorm(100,mean=25,sd=4)->HundredvaluesNormal
rnorm(500,mean=25,sd=4)->FiveHundredvaluesNormal

par(mfrow=c(2,2))
hist(HundredvaluesNormal)
hist(FiveHundredvaluesNormal)

shapiro.test(HundredvaluesNormal)
shapiro.test(FiveHundredvaluesNormal)

#get random numbers from a Poisson distribution with mean 4
rpois(100,lambda = 4)->HundredvaluesPois
rpois(5000,lambda = 4)->FiveHundredvaluesPois

hist(HundredvaluesPois)
hist(FiveHundredvaluesPois)





shapiro.test(HundredvaluesPois)
shapiro.test(FiveHundredvaluesPois)

#get random numbers from a Uniform distribution
runif(100)->HundredvaluesUnif
runif(500)->FiveHundredvaluesUnif

hist(HundredvaluesUnif)
hist(FiveHundredvaluesUnif)

shapiro.test(HundredvaluesUnif)
shapiro.test(FiveHundredvaluesUnif)

#install skellam package to look at skellam distribution
install.packages("skellam") # done only once
# difference tw two possin distribution
library(skellam) # done everytime R is started
set.seed(19)

rskellam(100, lambda1= 4, lambda2 =5)->HundredvaluesSkell
rskellam(500, lambda1= 4, lambda2 =5)->FiveHundredvaluesSkell

hist(HundredvaluesSkell)
hist(FiveHundredvaluesSkell)

shapiro.test(HundredvaluesSkell)
shapiro.test(FiveHundredvaluesSkell)



#using a for loop ----
for (i in 1:10)
{
  print (i)
}

for (i in 10:1) {
  print(i)
}

# for loop vs conditional statements
for (i in 1:10) {
  if (i %in% c(1, 3, 5, 7, 9)) {
    print(i)
  }
  # ifelse (i %in% c(2,4),print(i*2),print(0))
  #  else{if(i %in% c(6,8)){print("other")}}
}
pVals<-c()
WVals<-c()
#illustration of central limit theorem
set.seed(19)
#Get a uniform distribution of 5000 values
z = runif(5000)
#Using a for loop
for (i in 1:50)
{
  #add conditional statement here
  #if the value is between 1 and 5, add a exponential distr
  #else if the value is between 6 and 10, add a posion dist
  z = z+runif(5000)     #add another uniformly distributed variable 
  t = shapiro.test(z)
  pVals[i] = t$p.value
  WVals[i] = t$statistic
}
hist(z)
plot(c(1:10),pVals,type="b",xlab='Iteration')
plot(c(1:10),WVals,type="b",xlab='Iteration')

pvals<- c()
wvals<- c()
z <- rexp(5000)
for (i in 1:100) {
  if (i %in% 1:50) {
    z <- z+ rexp(5000)
  } else if (i %in% 60:100) {
    z <- z+ rpois(5000, lambda = 4)
  }
  t = shapiro.test(z)
  pvals[i] <- t$p.value
  wvals[i] <- t$statistic
}
plot(c(1:100),pvals,type="b",xlab='Iteration')
abline(h=0.05,col="red")
plot(c(1:10),pVals,type="l",xlab='Iteration')
plot(c(1:10),pVals,type="p",xlab='Iteration')

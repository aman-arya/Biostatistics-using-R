2+4

#here agument in radian 
cos(90)
#to know about the functions
# Trig {base} is a base pakckages
?cos
# some conts are define in R
pi
#convert deg to radian
(90*pi)/180
#rad to degree via function
rad2deg<-function(x){
  round((x*180/pi)%%360,0)
}
rad2deg(1)

# deg to rad function
deg2rad<-function(x){
  round((x*pi/180)%%360,0)
}
deg2rad(90)
cos(deg2rad(90))
cos(1.570796)

90*180/pi
# round off 
round(pi,2)
round(pi,4)
#data type 
as.character(1)
as.numeric(1)
#multi variable function
sumof2numbers<-function(x,y){
  x+y
}
sumof2numbers(2,3)
#
# CREATING OBJECTS in R ----
a <- 10 * 6
a<-6
6->a
a

A <- 2 + 6 # CASE SENSISTIVE
A
a

# OVERWRITING OBJECTS ----
a <- 0
a
#array of letters 
LETTERS
#postion 1 of array  note here indice start from 1
LETTERS[1]
#vector of integers from 1 to 20
c(1:20)->a
?c # here c is a function 
#print a
a
month.name
??monthDays
?monthdays # not working 

#NAMING OBJECTS ----
# not correct way for define object
1object <- 3
!object <- 3
-object <- 3
object! <- 3
#correct way to define object
object1 <- 3
my.object <- 3
my_object <- 3
myObject <- 3

#REMOVING OBJECTS ----
#rm(a)
rm(object1)

# DATA CLASSES ----
12.6    # NUMERIC
"Male"  # CHARACTER
TRUE    # LOGICAL

# DATA STRUCTURES ----
# VECTOR
c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
c(1:10)
c("Mon", "Tue", "Wed", "Thur", "Fri")
c(Mon, Tue, Wed, Thur, Fri)           # CHARACTER STRINGS MUST BE IN QUOTES
c(FALSE, TRUE, TRUE, FALSE, FALSE)
c(false, true)                        # LOGICAL MUST BE IN CAPITALS
myVector <- c(F, F, F, T, T)          # BUT CAN BE ABBREVIATED
myVector
numbers <- c(1:10)
numbers * 2                           # OPERATIONS CAN BE PERFORMED ON VECTORS
numbers + 3
numbers + numbers

# LIST
list(1, 2, 3, "hello", FALSE)
list(myVector, 1, 2, 3, "hello", FALSE) # LISTS CAN CONTAIN LISTS AND VECTORS
list(myVector, c(1, 2, 3), "hello", FALSE)


# DATA.FRAME (COME BACK TO)
# MENTION MATRICES AND ARRAYS BRIEFLY

# SUBSETTING VECTORS ----
days <- c("Mon", "Tue", "Wed", "Thur", "Fri")
days
days[1]
days[4]
days[c(1, 3, 4)]
days[1:4]
days[-5]
# FUNCTIONS ----
myValues <- c(1:100)
myValues
mean(myValues)
median(myValues)
min(myValues)
max(myValues)
sum(myValues)
sd(myValues)
class(myValues)
length(myValues) # SOME FUNCTIONS RETURN SINGLE VALUES (AGGREGATE FUNCTIONS)
log(myValues)    # OTHERS RETURN A VALUE FOR EACH COMPONENT OF THE VECTOR
log10(myValues)  # CAREFUL: DIFFERENCE BETWEEN LOG10 AND LOG
mySqrt <- sqrt(myValues)
mySqrt
?rnorm           # HELP ON HOW TO USE A FUNCTION
# normal distribution of 100 sample with a mean 5
rnorm(100, mean = 5)
hist(rnorm(100, mean = 5))
hist(rnorm(100, mean = 58))

# DATA.FRAMES ----
id <- (1:200)
group <- c(rep("Vehicle", 100), rep("Drug", 100))
group2<-rep(c(rep("vehicle",50),rep("drug",50)),2)
response <- c(rnorm(100, mean = 25, sd = 5), rnorm(100, mean = 23, sd = 5))
myData <- data.frame(Patient = id, Treatment = group, Response = response)
myData # CTRL+L
head(myData) # REMIND DATA WILL BE DIFFERENT BECAUSE OF RANDOM SEED
head(myData, 12)
tail(myData, 10)
dim(myData)
str(myData)
summary(myData)
View(myData) # to view in csv format
###############################Day 3###############################
group2


# SUBSETTING DATA.FRAMES ----
myData[1, 2] #[ROWS, COLUMNS]
myData[2, 3]
myData[1:20, 2:3]
myData[1:20, ]
myData[, 3]
myData[, "Response"]
myData$Response
myData[myData$Response > 26, ]
myData[myData$Treatment == "Vehicle" & myData$Response <= 23, ]
myData[myData$Treatment == "Vehicle" | myData$Response >= 21, ]
myData[myData$Treatment != "Vehicle" | myData$Response > 24, ]
age <- round(rnorm(200, mean = 40, sd = 20))
myData$Age <- age
head(myData)

# READING DATA INTO R ----
#Goto the correct directory
getwd()

setwd("/home/iiserb/Downloads")#This will not work 
setwd("/home/iiserb/Downloads")#But this will work change slase

getwd()

lifeExpectancyInYears <- read.csv("life_expectancy_years.csv", header = T) 


dim(lifeExpectancyInYears)
head(lifeExpectancyInYears)
str(lifeExpectancyInYears)
summary(lifeExpectancyInYears)

#Using column names as it is
lifeExpectancyInYears <- read.csv("life_expectancy_years.csv", header = T, check.names = FALSE) 

# Plotting Data ---

# PLOT of country and life expectancy in 1800
plot(lifeExpectancyInYears[,2])

# lifeExpectancyInYears[162,2:302] is life expectancy of particular rows(162) or particular country from 1800 to 2100
# we convert the above data by changing to a vector which is done first by unlist that list form of the data
# plot is country 162 vs column name that is years

lifeExpectancyInYears[162,1]#country sweden
plot(as.vector(unlist(lifeExpectancyInYears[162,2:302])),as.vector(unlist(colnames(lifeExpectancyInYears)[2:302])),xlab="Life expectancy in years", ylab="Year")

#life in 1800 and 1900 of all coutries
plot(lifeExpectancyInYears[,2],lifeExpectancyInYears[,101])
plot(lifeExpectancyInYears[,2],lifeExpectancyInYears[,3],xlab="1800", ylab="1801")

#plot of two counties life across all years
plot(as.vector(unlist(lifeExpectancyInYears[162,2:302])),as.vector(unlist(lifeExpectancyInYears[3,2:302])))

#subset to life expectancy in Sweden
lifeExpectancyInYears[lifeExpectancyInYears$country=="Sweden",]
# save life expectancy of sweden and india in a vecor by using unlist then convert to numeric then omit all na values 
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Sweden",])))->SwedenLife
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="India",])))->IndiaLife
# par function use to create partion of 1 rows and two columns
par(mfrow=c(1,2))
#plot histogram in that partion
hist(SwedenLife)
hist(IndiaLife)
#pch = symbols in graph 
#ylim is points in y axis

plot(c(1800:2100),SwedenLife,col="red", pch=15 , ylim=c(1,100),xlab="1800", ylab="1801")
points(c(1800:2100),IndiaLife, col="blue")
na.omit(as.numeric(unlist(lifeExpectancyInYears[lifeExpectancyInYears$country=="Bangladesh",])))->BanglaLife

plot(c(1800:2100),SwedenLife,col="red",pch=15,ylim=c(0,100),xlab="Years",ylab="Age")
points(c(1800:2100),IndiaLife,col="blue")
points(c(1800:2100),BanglaLife,col="green")

par(mfrow=c(3,1))
plot(c(1800:2100),SwedenLife,col="red",pch=15,ylim=c(0,100),xlab="Years",ylab="Age")
plot(c(1800:2100),IndiaLife,col="blue")
plot(c(1800:2100),BanglaLife,col="green")
#legengs we can add also in graphs
legend(X="topleft",   #postion
       legend = c("Sweden","India","Bangldesh"), #legends text
       fill = c("red", "blue","green")) #legends color

#abline is function used to make line in graphs

abline(v=2000)
abline(h=20)
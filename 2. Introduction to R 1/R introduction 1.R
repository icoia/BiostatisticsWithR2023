###################################### 
########################## R Introduction 1
# Calculation
3 + 4 #Sum
2 * 6 #multiple
2 ^ 4 #Exponent
2**4
log(10)

# assign name
a <- 2
b <- 3
a+b

c <- "Kian"

#we can assign name to objects just with letters, numbers, period (.), and underscore (_)

#R is case sensitive
t<- 1
T<- 2

# cannot use especial characters for object names (@,/,#,$)
@<- 1


# Use  period (.) or underscore (_) instead of space
annual_temp<- 28.5
annual.temp<- 28.5
# Short and explicit names are preferred.


# Object names have to strat with letters
1temp<-1




# see the list of Objects (in workspace) you made before
ls()
objects() # alternative

# see the folder of working directory (WD)
getwd()

# set WD
setwd("C:/Users/My Name/Desktop/Projects/multivariate/data")

# Install packages
install.packages("swirl")

# See installed packages
installed.packages()

# Learn more about packages
help(install.packages)
?mean
?install.packages
?? sequence


#run packages
library(swirl)
require(swirl)
library("vegan")




# show object content
radius <- 4

print(radius)

#calculating circle area
circle_area <-  pi * radius^2 # pi has the value of pi value = 3.14159
circle_area


# R has some default constants (?Constants)
pi
letters
LETTERS
month.name



# quit R
q()

# remove objects
rm(radius)
ls() #show all objects


rm(list=ls(all.names=TRUE)) # remove all variables




# Create a Vector
blood_sugar_control <- c(130,135,145,139,142)
blood_sugar_Treatment <- c(151,160,171,162,150)
blood_types <- c("AB","A","B","O") # character



# The rep( , ) function takes two arguments and is used to make multiple copies of the first argument.
# The first argument may be either a vector or a scalar.
rep(c(165,170), 3)
rep(165,6)
c(1, rep(170, 3), 146)
rep( c(170, 120), each = 4) # "each" = option makes copies of individual elements of the list

#sequence (from,to,by)
seq(150,160,2)

# The colon (:) operator produces an increasing or decreasing sequence of integers
90 : 100
100 : 90
90 : 100 / 4 #These can be combined in arithmetical expressions giving
blood_sugar <- c(blood_sugar_control, 2 * (60 : 65)) # combine arithmetic operations, sequences, and concatenation
blood_sugar

# parenthesis will also show the resulting value
(blood_sugar1 <- c(blood_sugar, 2 * (60 : 65))) # combine arithmetic operations, sequences, and concatenation


# show individual elements in a vector using square brackets [ ]
blood_sugar1
blood_sugar1[8]
blood_sugar1[4 : 7]
blood_sugar1[ c(1, 5, 6) ]


#omit the element of a vector
blood_sugar1
blood_sugar1 <- blood_sugar1[-3]
blood_sugar1 <- blood_sugar1[-c(1:5)]
blood_sugar1 <- blood_sugar1[-c(1,4,5)]


# The length() function tells us the length of a vector
length(blood_sugar1)
length(blood_sugar_control)


# The class() function tells us the class of variables
class(blood_types)
class(blood_sugar)



# logical
Logic<- blood_sugar1 <=124
Logic
class(Logic)

# <= less than or equal to
# >= Greater than or equal to
# < less than
# > greater than
# == equal to
# != not equal to


# subset (filter)
blood_sugar1[blood_sugar1 >=125] # show number greater than 125


# see the arguments of
#seq(from,to,by)
log(10,base= 10) # or
log(10,10) 
args(log)  


?log
? seq




# datasets exist in R
data()
data(women)

data(PlantGrowth)

class(PlantGrowth)
dim(PlantGrowth) # number of rows and columns (In order)
str(PlantGrowth) #structure of dataframe
names(PlantGrowth) # show name of columns
head(PlantGrowth) # show the first 6 rows
nrow(PlantGrowth) # number of rows
ncol(PlantGrowth) # number of columns


# select the columns
Plant_Weight<-PlantGrowth$weight
Plant_Weight

#choose columns
PlantGrowth$group

# calculating mean
mean(PlantGrowth$weight)

# convert data frame's columns into vector
Control <- PlantGrowth$weight[PlantGrowth$group=="ctrl"]
Treatment1 <- PlantGrowth$weight[PlantGrowth$group=="trt1"]
Treatment2 <- PlantGrowth$weight[PlantGrowth$group=="trt2"]


#descriptive statistics 
mean(Control) # average
median(Control) # Median
quantile(Control) # quartile
quantile(Control,0.72) # different percentile
IQR(Control) #Interquartile
min(Control)# minimum
max (Control) # maximum
range(Control) # show both minimum and maximum
max(Control) - min(Control)# show range
sd(Control) # standard deviation
var(Control) # Variance
summary(Control)
length(Control) # number of values
sqrt(4) # square root
Standard_error<-sd(Control)/sqrt(length(Control)) # standard error
# Skewness and Kurtosis
install.packages("moments")
library(moments)
skewness(Control)
kurtosis(Control)



# histogram
hist(Control)


# boxplot
boxplot(weight ~ group, data=PlantGrowth)



#Home Exercise
# ToothGrowth dataset
data(ToothGrowth)
# The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs.
#Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) 
#by one of two delivery methods, orange juice (OJ) or ascorbic acid (a form of vitamin C and coded as VC).

####Format
#A data frame with 60 observations on 3 variables.

#len:	length of odontoblasts (cells responsible for tooth growth)
#supp:	Supplement type (VC (vitamin C) or OJ (Orange juic)).
#dose: 	Dose in milligrams/day
# Reference: Crampton, E. W. (1947). The growth of the odontoblast of the incisor teeth as a criterion of vitamin C intake of the guinea pig. The Journal of Nutrition, 33(5), 491-504. doi: 10.1093/jn/33.5.491.


# descriptive statistics for OJ and VC groups
# Histogram for OJ and VC groups (Normal distribution?)
# boxplot (x axis for OJ and VC)
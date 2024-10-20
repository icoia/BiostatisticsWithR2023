# Previous Excercises
#exercise
#cbind() function ?????
# Creating matrix function????
# Control: 120,118,118,112,117,114
# Drug 1:  100,100,110,102,104,105
# Drug 2:  90,82,87,95,110,100
# create data frame with this data
#effect of drug on Blood pressure? (Data analysis)
#add new columns as log of blood pressure (LOG_Blood_pressure)



#cbind function - merge vectors to dataframe as columns
data("PlantGrowth")
Numbers<- c(1:30) # create a vector
PlantGrowth <- cbind (PlantGrowth, Numbers) # cbind (dataframe, vector1, vector2, vector3)


#create a matrix
matrix (c(12,15,18,19,33,56),3,2, byrow = FALSE) # matrix(numbers, number of rows, number of columns,filling columns by columns)
matrix (c(12,15,18,19,33,56),3,2, byrow = TRUE) # matrix(numbers, number of rows, number of columns, filling rows by rows)


# create data frame with this data:
Blood_pressure <- data.frame( Group= c(rep("control",6), rep("Drug1",6), rep("Drug2",6)), Blood_pressure= c(120,118,118,112,117,114,100,100,110,102,104,105,90,82,87,95,110,100) )

#add new columns as log of blood pressure (name of data.frame, name= value of new columns)
library(dplyr)
Blood_pressure <- mutate(Blood_pressure, Log_Blood_Presure= log(Blood_pressure) )












##### t-test 


rm(list=ls(all.names=TRUE)) # remove all objects (variables)



# Read file: Tooth Growth #
data(ToothGrowth) #load data
str (ToothGrowth) # structure

# Null Hypothesis: There is no significant difference between OJ and VC on tooth length
# Alternative Hypothesis: There is  significant difference between OJ and VC on tooth length


#convert categorical group  into factor (If it is character or numeric)
ToothGrowth$supp<-as.factor(ToothGrowth$supp)
str (ToothGrowth) # structure


# create separate vectors for different groups
orange_juice <-ToothGrowth$len [ToothGrowth$supp=="OJ"]
vitamin_C <-ToothGrowth$len [ToothGrowth$supp=="VC"]


################################
# Test for normal distribution _ Shapiro-Wilk test
shapiro.test(orange_juice)
shapiro.test(vitamin_C)

####################################
# QQ-plots, histograms and boxplot #
layout(matrix(c(1:6),nrow=3,ncol=2,byrow = TRUE)) # make new window with 6 free space (3 rows, 2 columns)

#QQ-plots
qqnorm(orange_juice,main="Normal Q-Q Plot of orange juice");qqline(orange_juice)
qqnorm(vitamin_C,main="Normal Q-Q Plot of vitamin C");qqline(vitamin_C)

# histogram with density line
hist(orange_juice,freq=FALSE) 
class_OJ<-seq(min(orange_juice),max(orange_juice),length.out=100) #length.out=number of produced 
lines(class_OJ,dnorm(class_OJ,mean(orange_juice),sd(orange_juice)))

hist(vitamin_C,freq=FALSE)
class_VC<-seq(min(vitamin_C),max(vitamin_C),length.out=100)
lines(class_VC,dnorm(class_VC,mean(vitamin_C),sd(vitamin_C)))


#boxplot
boxplot(len~supp,data=ToothGrowth,ylab="Tooth Length",xlab="supp", col="red")


#change order of levels (OJ and VC)
ToothGrowth$supp <- factor(ToothGrowth$supp, levels=c("VC", "OJ"))
boxplot(len~supp,data=ToothGrowth,ylab="Tooth Length",xlab="")



# Test for equal variances (Leven's test)
#install.packages("car")
library(car)
leveneTest(len~supp,data=ToothGrowth)



# Transformation (sqrt)
ToothGrowth$sqrt_len <- sqrt(ToothGrowth$len)


# create separate vectors regarding sqrt_len for different groups
sqrt_orange_juice <-ToothGrowth$sqrt_len [ToothGrowth$supp=="OJ"]
sqrt_vitamin_C <-ToothGrowth$sqrt_len [ToothGrowth$supp=="VC"]




# Test for normal distribution _ Shapiro-Wilk test
shapiro.test(sqrt_orange_juice)
shapiro.test(sqrt_vitamin_C)


# Test for equal variances (Leven's test)
#install.packages("car")
library(car)
leveneTest(sqrt_len~supp,ToothGrowth)



########################################################################

# t-tests for equal and unequal variances 
t.test(len~supp,data=ToothGrowth,var.equal=TRUE) # Equal variance
t.test(len~supp,data=ToothGrowth,var.equal=FALSE) # Unequal variance




############################################################ 
############################################################ 
# t-test for paired samples 
rm(list=ls(all.names=TRUE)) # remove all (objects) variables

# 15 patients take a drug for blood pressure, any effect?

#############
# Read file #
drug<-read.table("Paired t-test.txt",header=TRUE)
before<-drug$before
after<-drug$after

################################
# Test for normal distribution 
shapiro.test(after-before)

#########################################
# Quantile plots, histogram and boxplot #
layout(matrix(c(1:4),2,2,byrow = TRUE)) # make new windows

#QQ-plots
qqnorm(after-before,main="Normal Q-Q Plot of after-before");qqline(after-before)


#histogram
hist(after-before,freq=FALSE)
class<-seq(from=min(after-before),to=max(after-before),length.out=100)
lines(class,dnorm(class,mean(after-before),sd(after-before)))



##########################################
# t-test for paired (=dependent) samples #
t.test(after,before,paired=TRUE)





# Exercise:
#compare the difference between control and treatment1 in PlantGrowth dataset  (unpaired t-test)
# compare the difference between control and drug1 regarding blood pressure data frame (you made it previous session)  (Paired t-test)
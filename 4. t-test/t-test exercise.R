# Exercise 1: compare the difference between control and treatment1 in PlantGrowth dataset  (unpaired t-test)


rm(list=ls(all.names=TRUE)) # remove all objects (variables)



# Read file: Tooth Growth #
data("PlantGrowth") #load data
str (PlantGrowth) # structure




# create separate vectors for different groups
control <-PlantGrowth$weight [PlantGrowth$group=="ctrl"]
treatment <-PlantGrowth$weight [PlantGrowth$group=="trt1"]


################################
# Test for normal distribution _ Shapiro-Wilk test
shapiro.test(control)
shapiro.test(treatment)


#QQ-plots
qqnorm(control,main="Normal Q-Q Plot of control");qqline(control)
qqnorm(treatment,main="Normal Q-Q Plot of vitamin C");qqline(treatment)

# histogram with density line
hist(control,freq=FALSE) 
class_C<-seq(min(control),max(control),length.out=100)
lines(class_C,dnorm(class_C,mean(control),sd(control)))

hist(treatment,freq=FALSE)
class_T<-seq(min(treatment),max(treatment),length.out=100)
lines(class_T,dnorm(class_T,mean(treatment),sd(treatment)))


#boxplot
boxplot(weight~group,data=PlantGrowth,ylab="Leaf weight",xlab="")



# Test for equal variances (Leven's test)
#install.packages("car")
library(car)
leveneTest(weight~group,data=PlantGrowth)



# t-tests for equal and unequal variances 
t.test(control,treatment,var.equal=TRUE) # Equal variance
###############################################################################


# Exercise 2: compare the difference between control and drug1 regarding blood pressure data frame (you made it previous session)  (Paired t-test)
rm(list=ls(all.names=TRUE)) # remove all objects (variables)


# create data.frame (Blood pressure)
bp<- data.frame( Group= c(rep("control",6), rep("Drug1",6)), Blood_pressure= c(120,118,118,112,117,114,100,100,110,102,104,105) )


# create vectors
control <-bp$Blood_pressure [bp$Group=="control"]
Treatment <-bp$Blood_pressure [bp$Group=="Drug1"]


# Test for normal distribution 
shapiro.test(Treatment-control)


#QQ-plots
qqnorm(Treatment-control,main="Normal Q-Q Plot of after-before");qqline(Treatment-control)



# Boxplot 
boxplot(Blood_pressure~Group,data=bp,ylab="Blood Pressure",xlab="")


# t-test for paired (=dependent) samples
t.test(Treatment,control,paired=TRUE)



############################################################################################
#Exercise ==> ToothGrowth
#A data frame with 60 observations on 3 variables.

#len:	Tooth length
#supp:	Supplement type (VC (vitamin C) or OJ (Orange juic)).
#dose: 	Dose in milligrams/day
# Reference: Crampton, E. W. (1947). The growth of the odontoblast of the incisor teeth as a criterion of vitamin C intake of the guinea pig. The Journal of Nutrition, 33(5), 491-504. doi: 10.1093/jn/33.5.491.


data(ToothGrowth) # open the dataset frm R
str(ToothGrowth) #structure of dataframe
head(ToothGrowth) # show the first 6 columns
names(ToothGrowth) # show name of columns



#Transfer different group into vector
VC <- ToothGrowth$len[ToothGrowth$supp=="VC"]
OJ <- ToothGrowth$len[ToothGrowth$supp=="OJ"]


#descriptive statistics (VC)
mean(VC) # average
summary(VC)
sd(VC) # standard deviation



#descriptive statistics (OJ)
mean(OJ) # average
summary(OJ)
sd(OJ) # standard deviation

# histogram
hist(OJ)
hist(VC)


# boxplot
boxplot(len  ~ supp, data=ToothGrowth)
boxplot(len  ~ supp,ToothGrowth)
############################################################################################


#import file to R
BeeSeed <- read.table("Dataset.csv", sep=";", header=TRUE)
?read.table


# sep=",", sep=":", sep=";", sep="\t," 
str(BeeSeed) #structure of dataframe
#Art: Plant Species
#Seeds: Seed per flower
#Treatment: grouping factor ==> Control and different bee species


# Convert character to factor
BeeSeed$Treatment<- as.factor(BeeSeed$Treatment)
BeeSeed$FC_Treatment<- as.factor(BeeSeed$Treatment)
str(BeeSeed) #structure of dataframe


# Convert Factor to Character
BeeSeed$Treatment<- as.character(BeeSeed$Treatment)
str(BeeSeed) #structure of dataframe


# Convert character to factor
BeeSeed$Treatment<- as.factor(BeeSeed$Treatment)
str(BeeSeed) #structure of dataframe


# Convert integer to numeric 
BeeSeed$Seeds<- as.numeric(BeeSeed$Seeds)
str(BeeSeed) #structure of dataframe


# Convert Numeric to Integer 
BeeSeed$Seeds<- as.integer(BeeSeed$Seeds)
str(BeeSeed) #structure of dataframe


#Change values in a column
BeeSeed$Seeds<-2*BeeSeed$Seeds
BeeSeed$Seeds<-BeeSeed$Seeds/2
str(BeeSeed)
BeeSeed$Twice_Seeds <- 2*BeeSeed$Seeds # create a new column

# package for data manipulation
#install.packages("dplyr")
library(dplyr)


#add column to our data.frame- mutate(name of data.frame, name= value of new columns)("dplyr")
BeeSeed <- mutate(BeeSeed, Total_Seed=Seeds*10)
head(BeeSeed) # show first six rows

# subset 
BeeSeed2 <- subset(BeeSeed, (Treatment != "LM") & (Treatment != "CS"))
BeeSeed3 <- subset(BeeSeed, (Treatment == "control")) 



# filter data - filter (data.frame name, filter index ) (dplyr)
BeeSeed<-filter (BeeSeed,Total_Seed >= 100)


# select a columns you need (data,frame, column1,......) (dplyr)
BeeSeed4 <- select (BeeSeed,  Seeds )

# create vector from one column in dataframe
BeeSeed5 <- BeeSeed $Seeds

#combine functions and do in sequence - %>% (shortcut: ctrl+ shift+ M) (dplyr)
data(ToothGrowth)
Tooth <- ToothGrowth  %>% select (len,supp ) %>% filter (len >= 10) %>% mutate(color=len*10)




# create data.frame (it's not related to package dplyr)
Blood_Sugar <- data.frame( Group= c("Control","Control", "Control", "Control", "Control",  "Treatment","Treatment","Treatment","Treatment","Treatment"), Blood_Sugar= c(162,167,159,170,166,178,184,188,192,190)) # alternative 1
class(Blood_Sugar)
Blood_Sugar <- data.frame( Group= c(rep("Control",5), rep("Treatment",5)), Blood_Sugar= c(162,167,159,170,166,178,184,188,192,190) ) # alternative 2
str(Blood_Sugar)    


# show data in certain column and row [row,column]
Tooth[3,2] # row 3, column 2
Tooth[3,1] # row 3, column 1
Tooth [,1] # column 1
Tooth [1,] # row 1
Tooth [1] # column 1 (with header)
tooth_length <- Tooth [c(1,2,3)] # column 1 and 2

# remove column or row
data("ToothGrowth")
ToothGrowth <- ToothGrowth[,-3] # remove column 3
ToothGrowth1 <- ToothGrowth[-10,] # remove row 10

# rename columns as header
ToothGrowth <-rename(ToothGrowth,Length = len,Nutrition = supp)
names(ToothGrowth) # show headers

# sort data 
data(PlantGrowth)
order(PlantGrowth$weight) #Ascending
order(-PlantGrowth$weight) # Descending
PlantGrowth <-PlantGrowth[order(PlantGrowth$weight),] #from smallest to biggest
PlantGrowth <-PlantGrowth[order(-PlantGrowth$weight),] #from biggest to smallest


# replace value with another value
data("ToothGrowth")
ToothGrowth[ToothGrowth == 4.2] <- 5


# replace character with another character
str(ToothGrowth) # show structure of dataframe (df)
ToothGrowth$supp<- as.character (ToothGrowth$supp) # convert to character
str(ToothGrowth)

ToothGrowth[ToothGrowth == "OJ"] <- "Orange juice"
ToothGrowth[ToothGrowth == 0.5] <- NA



# which function (shows which entries are true)
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which (x)


#install.packages ("dslabs")
library (dslabs)
data(murders)
str(murders)


# shows Massachusetts population
murders$population [which(murders$state== "Massachusetts")] 


# in detail
murders$population
murders$state== "Massachusetts"
which(murders$state== "Massachusetts")
murders$population[22]

#exercise
#1: cbind() function ?????

#2:  Creating matrix function????

#3:
# Control: 120,118,118,112,117,114
# Drug 1:  100,100,110,102,104,105
# Drug 2:  90,82,87,95,110,100
# create data frame with this data
#effect of drug on Blood pressure? (Data analysis)
#add new columns as log of blood pressure (header: LOG_Blood_pressure)
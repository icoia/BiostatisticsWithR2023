rm(list=ls(all=TRUE)) # remove all previous objects


# Load Packages
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
          

# Load the "Enzyme" file
Enzyme <- read.table("Enzyme.csv", sep=",", header=TRUE)
str(Enzyme) # Structure
Enzyme$Soil_Type <- factor(Enzyme$Soil_Type, levels=c("Unfertilized","Chemical", "Organic" )) # change the order
levels(Enzyme$Soil_Type) # see the levels

# Load the "Heights" file
#install.packages("dslabs")
library(dslabs)
data(heights) # height of people in inches




############################################## QQ-plot
#install.packages("ggpubr")
library(ggpubr)
ggqqplot(heights, x = "height", color = "blue")



############################################## Box plot


# simple box plot
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase)) + 
  geom_boxplot() +
  xlab(" ") +
  ylab("Soil Chitinase ") 


# change outliers in box plot
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") 



# Show the mean in box plot
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") +
stat_summary(fun=mean, geom="point", shape=23, size=4)



 # Rotate the box plot
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
coord_flip()



# Box plot with dots
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  geom_jitter(shape=16)


# change color of outline
ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase, color=Soil_Type)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") +
  stat_summary(fun=mean, geom="point", shape=23, size=4) + 
scale_color_manual(values=c("#7D7878", "#518F42", "#FEDA6A"), name = "Soil type") 
# scale_color_manual: Choose color of outline



# change color of filled boxes
box <- ggplot(Enzyme, aes(x=Soil_Type, y=Cellulase, fill=Soil_Type)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4) +
  xlab(" ") +
  ylab("Soil Chitinase ") +
  stat_summary(fun=mean, geom="point", shape=23, size=4) +
  scale_fill_manual(values=c("#7D7878", "#518F42", "#FEDA6A"), name = "Soil type")
box
# scale_fill_manual: Choose color of filled boxes


# change position of legend
box + theme(legend.position="top") 
box + theme(legend.position="bottom")
box + theme(legend.position="none") # Remove legend



# rotate labels
box + theme(axis.text.x=element_text(angle=90,hjust=1))
# angle: angle of the x-axis labels
# hjust=the distance in y-axis
# axis.text.y: changing the labels in y-axis




####################################### Violin Plot
# violin plots are similar to box plots, except that
#they also show the probability density of the data at different values

data("ToothGrowth")
str(ToothGrowth) # structure
ToothGrowth$dose <- as.factor(ToothGrowth$dose) # Convert Variable"dose" into factor

# basic violin plot
p <-ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_violin(trim=FALSE)
p


# violin plot with box plot
p + geom_boxplot(width=0.1)


# violin plot with mean points
p + stat_summary(fun=mean, geom="point", shape=23, size=2, color="red")

# violin plot with median points
p + stat_summary(fun=median, geom="point", size=2, color="red")

# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1, fill="red") # with class intervals
#stackdir= "left", "center", "right"


p + geom_jitter(shape=16,fill="red", position=position_jitter(0.05),  colour = "red",   cex = 1) # Without class intervals
#cex: size of dots

# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin( trim=FALSE, fill='gray', color="darkred")+
  geom_boxplot(width=0.1) +
  theme_minimal()

# Change violin plot colors by groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_violin(trim=FALSE)

#################################################### Barplot
# Basic barplot
ggplot(data=Enzyme, aes(x=Soil_Type, y=Chitinase)) +
  geom_bar(stat="identity")
# If you want the heights of the bars to represent values in the data, use stat="identity"

# Change the width of bars
ggplot(data=Enzyme, aes(x=Soil_Type, y=Chitinase)) +
  geom_bar(stat="identity", width=0.5)





# Change the color of bars based on levels
ggplot(data=Enzyme, aes(x=Soil_Type, y=Chitinase, fill=Soil_Type)) +
  geom_bar(stat="identity", width=0.5) +
  scale_fill_manual(values=c("#7D7878", "#518F42", "#FEDA6A"), name = "Soil type")




# Barplot with standard error
ggplot(data=Enzyme, aes(x=Soil_Type, y=Chitinase, fill=Soil_Type)) +
stat_summary(fun=mean, geom="bar") +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2)





########################################################Stacked barplot

# change the format of dataframe
install.packages("tidyr")
library(tidyr)
Enzyme2 <- Enzyme %>% gather(Enz,Activity, 2:ncol(Enzyme))



# Stacked barplot
ggplot(Enzyme2, aes(x = Soil_Type, y = Activity, fill = Enz)) +
  geom_bar( fun = mean ,stat="summary") +
  scale_fill_manual(values=c( "#3fb565", "red",
                              "#3f65b5", "#e86be6", "brown", "yellow",
                              "orange")) +
  labs( x="", y="Enzyme activity" , fill="Enzyme") 



# Another form of Stacked barplot
ggplot(Enzyme2, aes(x = Soil_Type, y = Activity, fill = Enz)) +
  geom_bar( fun = mean ,stat="summary", position = "dodge") +
  scale_fill_manual(values=c( "#3fb565", "red",
                              "#3f65b5", "#e86be6", "brown", "yellow",
                              "orange")) +
  labs( x="", y="Enzyme activity" , fill="Enzyme") 





###########################################Faceting: 
#makes multiple side-by-side plots stratified by some variable

#install.packages("dslabs")
library(dslabs)
data(gapminder) # load dataset
str(gapminder) # structure



gapminder2 <- filter(gapminder, year %in% c(1962, 2012)) # Filter year 1962 and 2012

# facet by continent and year
  ggplot(gapminder2,aes(x=fertility, y=life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)


  # facet by year only
  ggplot(gapminder2,aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid( ~ year)

# facet by year, plots wrapped onto multiple rows
  gapminder %>%
    filter(year %in% c(1962, 1980, 1990, 2000, 2012) & continent %in% c("Europe", "Asia")) %>%
    ggplot(aes(x=fertility, y=life_expectancy, col = continent)) +
    geom_point() +
    facet_wrap(~year)
  
  
######################################### line plot

# scatterplot of US fertility by year
gapminder2 <- gapminder %>% filter(country == "Iran") # filter Iran
  
ggplot(gapminder2, aes(x=year, y=fertility)) +
geom_point()


# line plot of US fertility by year
ggplot(gapminder2,aes(x=year, y=fertility)) +
geom_line()
  
  
# Multiple lines

  
gapminder2 <- gapminder %>% filter(country %in% c("Iran", "Austria")) # Filter Iran and Austria (Not Australia)
  
ggplot(gapminder2, aes(x=year, y=fertility, col = country)) +
geom_line() 

  
# show points in line plots
ggplot(gapminder2, aes(x=year, y=fertility, col = country)) +
geom_line() +
geom_point() +
scale_color_manual(values=c("red", "blue"))




# change line types
ggplot(gapminder2, aes(x=year, y=fertility, linetype = country)) +
geom_line()  +
scale_linetype_manual(values=c("solid", "dotted"))




################################################## Heatmap



# create correlation table
cor <- cor(Enzyme[,2:ncol(Enzyme)])
cor <- round(cor,2) # round values to 2 decimals



# package reshape is required to melt the correlation matrix 
#install.packages("reshape2")
library(reshape2)
melted_cor <- melt(cor) # melt the correlation matrix
head(melted_cor) # show the first 6 rows



# Basic heatmap (Really ugly)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() 



#Better plot
ggplot(data = melted_cor, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 11, hjust = 1))+
  theme(axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 11, hjust = 1))+
  coord_fixed()  






# I'm fan of corrplot package



### Exercise:
# Use previous datasets to create different plots by ggplot
# how to create pie chart in ggplot2?
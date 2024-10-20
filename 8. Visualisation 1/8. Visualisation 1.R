rm(list=ls(all=TRUE)) # remove all previous objects


# Load Packages
#install.packages("ggplot2")
library(ggplot2)
?ggplot2

#install.packages("dplyr")
library(dplyr)
          



# Load the file
Enzyme <- read.table("Enzyme.csv", sep=",", header=TRUE)
str(Enzyme) # Structure


# convert Soil_Type into factor
Enzyme$Soil_Type <- as.factor (Enzyme$Soil_Type) 
levels(Enzyme$Soil_Type) # see the levels

# create scatter plot
p <-ggplot(aes(x=Cellulase, y=Chitinase), data= Enzyme)+
  geom_point()
p

class(p) # type of the object


# change the size and shape of the points 
ggplot(aes(x=Cellulase, y=Chitinase), data= Enzyme) + 
geom_point(size = 2, color = "blue", shape= 2)

# size: size of points
# colors: color of points
# shape= shape of points 
# find the shape code here: http://www.sthda.com/english/wiki/ggplot2-point-shapes
# see color code here: https://htmlcolorcodes.com/

#show labels
ggplot(aes(x=Cellulase, y=Chitinase, label=Soil_Type), data= Enzyme) + 
  geom_point(size = 2, color = "blue")+ 
  geom_text()





# move text labels slightly to the right with "nudge_x"
ggplot(aes(x=Cellulase, y=Chitinase, label=Soil_Type), data= Enzyme) + 
  geom_point(size = 2, color = "blue")+   
  geom_text(size=3,nudge_x = +15)

#nudge_x or nudge_y:
# -: left
# +: right



# Add labels and titles
ggplot(aes(x=Cellulase, y=Chitinase, label=Soil_Type), data= Enzyme) + 
  geom_point(size = 2, color = "blue")+   
  geom_text( nudge_x = 8) +
  xlab("Cellulase (unit)") +
  ylab("Chitinase (Unit)") +
  ggtitle("Enzyme correlation")
?ggtitle

# logarithm transformation
ggplot(aes(x= log(Cellulase), y=log(Chitinase), label=Soil_Type), data= Enzyme) + 
  geom_point(size = 2, color = "blue")+   
  geom_text( nudge_x = 0.05) +
  xlab("Cellulase (log)") +
  ylab("Chitinase (log)") +
  ggtitle("Enzyme correlation")

  
# set different color for soil Type
ggplot(aes(x= Cellulase, y=Chitinase, color=Soil_Type), data= Enzyme) + 
  geom_point(size = 2)+ 
  scale_color_manual(values=c("#7D7878", "#518F42", "#FEDA6A"), name = "Soil type") +
  xlab("Cellulase (log)") +
  ylab("Chitinase (log)") +
  ggtitle("Enzyme correlation")


# change the order of soil_Type
levels(Enzyme$Soil_Type) # see the levels
Enzyme$Soil_Type <- factor(Enzyme$Soil_Type, levels=c("Unfertilized","Chemical", "Organic" )) # change the order
levels(Enzyme$Soil_Type) # see the levels

# plot again the last plot ( see the order)
ggplot(aes(x= Cellulase, y=Chitinase, color=Soil_Type), data= Enzyme) + 
  geom_point(size = 2)+ 
  scale_color_manual(values=c("#7D7878", "#518F42", "#FEDA6A"), name = "Soil type") +
  xlab("Cellulase (log)") +
  ylab("Chitinase (log)") +
  ggtitle("Enzyme correlation")




# set different shape for soil Type
ggplot(aes(x= Cellulase, y=Chitinase, shape = Soil_Type), data= Enzyme) + 
  geom_point(size = 4, color = "black")+ 
  scale_shape_manual(values = c(17, 20,22), name = "Soil type") +
  xlab("Cellulase (log)") +
  ylab("Chitinase (log)") +
  ggtitle("Enzyme correlation")



  

# set regression line 
graph <- ggplot(aes(x= Cellulase, y=Chitinase, color=Soil_Type), data= Enzyme) + 
  geom_point(size = 2)+ 
  scale_color_manual(values=c("#7D7878", "#518F42", "#FEDA6A")) +
  xlab("Soil Cellulase ") +
  ylab("Soil Chitinase ") +
  ggtitle("Enzyme correlation")+ 
  geom_smooth(color="red", method=lm, se=FALSE, linetype="solid") 
?geom_smooth
graph

# see the linetypes here: http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software

summary(lm (Chitinase~Cellulase, Enzyme)) # see the linear regression info


# add themes
#install.packages("ggthemes")
library(ggthemes) # package for themes

graph + theme_economist()    # style of the Economist magazine
graph + theme_fivethirtyeight()    # style of the FiveThirtyEight website
graph + theme_light() # my favorite theme
# see more themes: https://ggplot2.tidyverse.org/reference/ggtheme.html


# Save the graph
graph + theme_light()
ggsave("Soil enzyme.jpg", width=6, height=5)
# save in different formats :png, pdf, jpg, tiff and so on




################# Histogram

#Load dataset
#install.packages("dslabs")
library(dslabs)
data(heights) # height of people in inches

# Convert inch to centimeter
heights$ height <- 2.54*(heights$ height)


# filter female hights (dplyr package)
heights <-  heights %>%
  filter(sex == "Female")


# basic histograms
ggplot(heights,aes(x = height)) +
  geom_histogram(binwidth = 10)
#binwidth: size of bin interval


# histogram with blue fill(blue bars), Red outline, labels and title
ggplot(heights,aes(x = height)) + 
  geom_histogram(binwidth = 15, fill = "blue", col = "red") +
  xlab("Female heights in cm") +
  ggtitle("Histogram")
# fill: filler color
# col: Outline color 


########### Organize different plots together with gridExtra package

# define plots p1, p2, p3
p1 <- ggplot(heights,aes(x = height)) + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- ggplot(heights,aes(x = height)) + geom_histogram(binwidth = 5, fill = "blue", col = "black")
p3 <- ggplot(heights,aes(x = height)) + geom_histogram(binwidth = 10, fill = "blue", col = "black")
p4 <- ggplot(heights,aes(x = height)) + geom_histogram(binwidth = 20, fill = "blue", col = "black")


# arrange plots next to each other in 1 row, 3 columns
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3,p4, nrow = 4,ncol=1)





######################### smooth density plot (probability distribution)
ggplot(heights,aes(x = height)) + 
  geom_density(fill = "blue", col = "red")+ 
  xlab("Female height")
# fill: filler color
# col: Outline color 


# Exercise:
# Creating Scatter plot with Iris dataset 
data(iris)









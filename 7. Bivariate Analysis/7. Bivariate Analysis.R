####### Correlation
rm(list=ls(all=TRUE)) # remove all previous objects




# Read file 
StemLeaf<-read.table("StemLeaf.txt",header=TRUE)
str(StemLeaf) # structure
# Is there a correlation between stem length and leaf area?


stem<-StemLeaf$stem
leaf<-StemLeaf$leaf

# check for at least approximate normal distribution 
qqnorm(stem);qqline(stem) # qqplot
qqnorm(leaf);qqline(leaf) # qqplot

boxplot(StemLeaf) # Boxplot

# Scatter plot
plot(leaf~stem)

# Pearson's correlation coefficient
cor.test(stem,leaf,method="pearson")


# Spearman's correlation coefficient (Non-parametric test)
cor.test(stem,leaf,method="spearman")





####### Correlation table
rm(list=ls(all=TRUE)) # remove all previous objects


# Load file
Enzyme <- read.table("Enzyme.csv", sep=",", header=TRUE)
str(Enzyme) # structure


# correlation table
cor <- cor(Enzyme, use="pairwise", method="spearman")
cor

# Convert correlation table into data frame
cor<-data.frame(cor)


#make the rownames into the first column of the matrix
cor <- data.frame(Variables = row.names(cor), cor) # create dataframe with rownames of correlation table
rownames(cor) = NULL # remove row names



# save the correlation table in W
#install.packages("writexl")
library (writexl)
write_xlsx(cor,"D:/Stat/Correlation_Table_Enzyme.xlsx") # save the correlation table




###### heatmap
# save the correlation table in WD
#install.packages("corrplot")
library (corrplot)


# correlation table
cor2<-cor(Enzyme,use="pairwise", method="spearman") # I made new file because Correlation table shouldn't be in data.frame 
class(cor2)

# create heatmaps
png(height=1200, width=1200, file="CorrelationPlot0.png")
corrplot(cor2, method = 'number', tl.cex=0.9, tl.col = '#59B05B') # colorful number
dev.off()
# tl.cex: size of the text
# tl.col: color of the text

png(height=1200, width=1200, file="CorrelationPlot1.png")
corrplot(cor2, method = 'color', tl.cex=0.8, tl.col = 'black') 
dev.off()


png(height=1200, width=1200, file="CorrelationPlot2.png")
corrplot(cor2,tl.cex=0.8, tl.col = 'black' )
dev.off()


png(height=1200, width=1200, file="CorrelationPlot3.png")
corrplot(cor2, method = 'shade',  diag = FALSE, tl.cex=0.9, tl.col = 'black')
dev.off()
# diag: if false, it removes the diagonals

png(height=1200, width=1200, file="CorrelationPlot4.png")
corrplot(cor2, method = 'square', type = 'lower', diag = FALSE, tl.cex=0.9, tl.col = 'black', tl.srt = 45)
dev.off()
# tl.srt: degree of texts
# tye: upper, lower or full


png(height=1200, width=1200, file="CorrelationPlot5.png")
corrplot(cor2, method = 'ellipse', type = 'upper', tl.cex=0.9, tl.col = 'black', tl.srt = 90)
dev.off()






########### Simple linear regression 
rm(list=ls(all=TRUE)) # remove all previous objects
bird <- read.csv("birdsdiet.csv", stringsAsFactors = TRUE)
str(bird)

#We hypothesized: 
#for different bird species, the average mass of an individual has an effect on the maximum abundance of the species,
#due to ecological constraints (food sources, habitat availability, etc.).



# Scatter plot
plot(MaxAbund ~ Mass,data=bird)
abline(lm(MaxAbund ~ Mass,data=bird)) # Generate a line on scatter plot



# Linear regression of maximum abundance against mass
lm1 <- lm(MaxAbund ~ Mass, data = bird)

# Examination of the regression output
summary(lm1)



# Plot the four diagnostic plots
par(mfrow = c(2, 2))
plot(lm1)


# check assumptions in detail
plot(MaxAbund ~ Mass, data = bird)  # left plot
abline(lm1)  # line defined by the model parameters
hist(residuals(lm1))  # distribution of residuals


# Test the normality of residuals
shapiro.test(residuals(lm1))


# Skewness test
#install.packages("e1071")
library(e1071)
skewness(residuals(lm1))



# log-transform the variables
bird$logMaxAbund <- log10(bird$MaxAbund)
bird$logMass <- log10(bird$Mass)


# Linear model with transformed data
lm2 <- lm(logMaxAbund ~ logMass, data = bird)
summary(lm2)


# Diagnostic plots for transformed data
par(mfrow = c(2, 2))
plot(lm2)



# check assumptions for transformed data (logMaxAbund ~ logMass, data = bird)
plot(logMaxAbund ~ logMass, data = bird)
abline(lm2)
hist(residuals(lm2))




#Let us formulate a new and more precise hypothesis. This time let’s focus only on land birds.


# Linear model with terrestrial birds
lm3<-lm(logMaxAbund~logMass, data=bird,subset=bird$Aquatic == 0)
summary (lm3)
names(birds)
# Diagnostic plots for lm3
par(mfrow = c(2, 2))
plot(lm3)
shapiro.test(residuals(lm3))


plot(logMaxAbund ~ logMass, data = bird)
abline(lm3)
hist(residuals(lm3))


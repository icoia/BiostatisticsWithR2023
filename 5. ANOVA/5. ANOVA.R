### One-way ANOVA
# Is number of seeds per plant  affected by different species of bees?
# H0: There is no difference between treatments.
# HA: At least one treatment has different seeds per plant?
rm(list=ls(all=TRUE)) # remove all previous objects

#############
# Read file #
BeeSeed<-read.table(file="Dataset.csv",sep=",",header=TRUE)
str(BeeSeed) # type of data

# convert Treatment from characte to factor
BeeSeed$Treatment <- as.factor(BeeSeed$Treatment)
str(BeeSeed) # type of data

# see the levels of treatments
levels(BeeSeed$Treatment)



# First check data: normal distribution? variance homogeneity? transformation? #
boxplot(Seeds~Treatment,data=BeeSeed)



# Test for normal distribution 
shapiro.test(BeeSeed$Seeds[BeeSeed$Treatment=="control"])
shapiro.test(BeeSeed$Seeds[BeeSeed$Treatment=="CS"])
shapiro.test(BeeSeed$Seeds[BeeSeed$Treatment=="LM"])
shapiro.test(BeeSeed$Seeds[BeeSeed$Treatment=="MM"])


# Test for homogeneity of variances (i.e. equal variances) #
bartlett.test(Seeds~Treatment,data=BeeSeed)


# one-way ANOVA (Homogeneity of variance)
fit<-aov(Seeds~Treatment,data=BeeSeed) 
summary(fit)

# post-hoc test
TukeyHSD(fit)



# Welch test (Unequal variance)
oneway.test(Seeds~Treatment,data=BeeSeed,var.equal=FALSE)

#post-hoc test
pairwise.t.test(BeeSeed$Seeds,BeeSeed$Treatment,p.adj="bonferroni",pool.sd=FALSE)






# Repeated measurement ANOVA


rm(list=ls(all=TRUE)) # remove all previous objects

#############
# Read file 
drug <-read.table(file="drug.csv",sep=",",header=TRUE)
str(drug) # type of data



# convert Treatment from characte to factor
drug$Time <- as.factor(drug$Time )
str(drug) # type of data
levels(drug$Time) # show groups (levels)


# boxplot
boxplot(Blood_Pressure~Time,data=drug)


# Test for normal distribution 
shapiro.test(drug$Blood_Pressure[drug$Time=="t0"])
shapiro.test(drug$Blood_Pressure[drug$Time=="t1"])
shapiro.test(drug$Blood_Pressure[drug$Time=="t2"])


#install.packages("rstatix")
library(rstatix)
res<-anova_test(data=drug,dv=Blood_Pressure,wid=ID, within=Time) 

# data: data frame
# dv: numeric variable name.
# wid: variable name specifying the case/sample identifier.
# within: grouping variable


get_anova_table(res) # see the results table


# post-hoc test
#install.packages("dplyr")
library(dplyr)
posthoc <- drug %>%  pairwise_t_test(Blood_Pressure ~ Time, paired = TRUE,p.adjust.method = "bonferroni")
posthoc





############## Two-way ANOVA
rm(list=ls(all=TRUE)) # remove all previous objects



# load data
actino <- read.table("actino.csv", sep=",", header=TRUE) # load the file
str(actino) # structure


# convert chemical and organic to factors and change the name of labels
# label levels
actino$Chemical <- factor(actino$Chemical, 
                      levels = c(0, 1),
                      labels = c("Chemical -", "Chemical +"))



actino$Organic <- factor(actino$Organic, 
                      levels = c(0, 1),
                      labels = c("Organic -", "Organic +"))




# Interaction plot
# +++++++++++++++++++++++
# Plot actinomycetes abundance by groups ("Organic")
# Color box plot by a second group: "Chemical"
# Add error bars: mean_se

#install.packages("ggplot2") 
library(ggplot2)
#install.packages("backports")
library(backports)
#install.packages("ggpubr")
library("ggpubr")
ggline(actino, x = "Organic", y = "Actino_ugC_DW", color = "Chemical",
       add = c("mean_se", "dotplot"),
       palette = c("blue", "red")) +
  labs(x = '', y = 'Soil Actinomycetes biomass (µg C g'^-1~'dw)')


# Two-Way ANOVA test
actino.aov <- aov(Actino_ugC_DW ~ Chemical * Organic, data = actino)
summary(actino.aov)

#########Check the assumptions
#Levene's test (Homogeneity of variance)
library(car)
leveneTest(Actino_ugC_DW ~ Chemical * Organic, data = actino)


# Normality across groups
library(ggplot2)
ggqqplot(actino, "Actino_ugC_DW", ggtheme = theme_bw()) +
  facet_grid(Chemical ~ Organic , labeller = "label_both")



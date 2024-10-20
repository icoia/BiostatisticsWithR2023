rm(list=ls(all=TRUE)) # remove all previous objects


#### Mann-Whitney U Test (independent 2-group)
data("ToothGrowth") # load data
str(ToothGrowth) # structure


#converting groups into vectors
OJ<-ToothGrowth$len[ToothGrowth$supp=="OJ"]
VC<-ToothGrowth$len[ToothGrowth$supp=="VC"]

# Mann-Whitney U Test
wilcox.test(OJ,VC) # Alternative 1
wilcox.test(len~supp, ToothGrowth) # Alternative 2






####### Wilcoxon Signed-Rank Test (dependent 2-group)
rm(list=ls(all=TRUE)) # remove all previous objects
drug <-read.table("Paired 2 groups.txt",header=TRUE) # load data
str(drug) # structure


#converting groups into vectors
Before <- drug$before
After <-drug$after


# Wilcoxon Signed-Rank Test
wilcox.test(Before,After,paired=TRUE) 







###### Kruskal-Wallis H test (Independent >2 groups)
rm(list=ls(all=TRUE)) # remove all previous objects

data("PlantGrowth") # load data
str(PlantGrowth) #structure


# Kruskal-Wallis H test
kruskal.test(weight~group,PlantGrowth)


# Multiple pairwise-comparisons
#install.packages("rstatix")
library(rstatix)
wilcox_test(weight ~ group,data=PlantGrowth, p.adjust.method = "bonferroni")









############ Friedman test
rm(list=ls(all=TRUE)) # remove all previous objects

# Load file
drug <-read.table(file="drug.csv",sep=",",header=TRUE)
str(drug) # type of data


# convert Treatment from characte to factor
drug$Time <- as.factor(drug$Time )
str(drug) # type of data
levels(drug$Time) # show groups (levels)


# boxplot
boxplot(Blood_Pressure~Time,data=drug)



# Friedman test
friedman.test(y=drug$Blood_Pressure, groups=drug$Time, blocks=drug$ID)


# Multiple pairwise-comparisons
pairwise.wilcox.test(drug$Blood_Pressure, drug$Time, p.adj = "bonf")













############### Chi-square test of independence

rm(list=ls(all=TRUE)) # remove all previous objects


#Load Data
data(iris)


# make new column called "size" - if size of sepal is smaller than sepal median : small - if not: big
iris$size <- ifelse(iris$Sepal.Length < median(iris$Sepal.Length), "small", "big") 




# create a contingency table
table(iris$Species, iris$size)
# The contingency table gives the observed number of cases in each subgroup


# Barplot
#install.packages("ggplot2")
library(ggplot2)
ggplot(iris) +
  aes(x = Species, fill = size) +
  geom_bar()



#Chi-square test of independence
chi_test <- chisq.test(table(iris$Species, iris$size))
chi_test



# Phi or Cramer's V
#install.packages("vcd")
library(vcd)
assocstats(table(iris$Species, iris$size))



######Exercise
# Chi-square test of independence for "Mari_Cancer" dataset
# Do non-parametric tests for previous exercises

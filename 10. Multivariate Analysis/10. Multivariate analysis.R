rm(list=ls()) # Remove Previous variables

# Load data 
microbial <- read.csv("Microbial Community.csv", sep=",", dec=".", header=TRUE, stringsAsFactors=TRUE)
#stringsAsFactors=TRUE : convert characters into factors

str(microbial) # structure
levels(microbial$Soil_Type) #See the levels





####### Correspondence Analysis (CA)
data.ca <- microbial


# Set NAs zero
data.ca[is.na(data.ca)] <- 0
head(data.ca)

# remove ascomycota as outlier
#names(data.ca) # see the column names
#data.ca <- data.ca[-4]


# Load packages
#install.packages("vegan")
library("vegan")
#install.packages("grid")
library("grid")

# Create CA file
mod <- cca(X = data.ca[, 2:ncol(data.ca)]) # X is the community data matrix
summary(mod)
? cca


# extract Site Scores from an Ordination
cca.sites <- data.frame(scores(mod, choices=c(1,2), display="sites", scaling=3))
?scores

# create dataframe with categorical variable and site scores(CA1 and CA2)
ord_df <- data.frame( Treatment=data.ca$Soil_Type,
                     CA1=cca.sites$CA1, CA2=cca.sites$CA2)

str(ord_df) # structure

# change Treatment variable into factor (If it is not factor)
#ord_df$Site <- factor(ord_df$Site)
#str(ord_df) # structure




# Extract the proportion of variance explained by each CA axis
exp <- summary(eigenvals(mod))
exp <- data.frame(exp) # convert into dataframe
exp <- subset(exp, rownames(exp)=="Proportion Explained") # keep just Proportion Explained data

# extract Species Scores from an Ordination
cca.species <- data.frame(scores(mod, choices=c(1,2), 
                                 display="species", scaling=3))

cca.species$microbial <- rownames(cca.species) # create new column with row names
head(cca.species) # see the first six rows



# Plot it! 
#install.packages("ggplot2")
library(ggplot2)

# change the order of levels
ord_df$Treatment <- factor(ord_df$Treatment, levels=c("unfertilized","Chemical", "Organic", "Lime"))
levels(ord_df$Treatment)

ggplot() +
  geom_point(data=ord_df, aes(x=CA1, y=CA2, colour=Treatment), size=2) +
  geom_polygon(data=ord_df, aes(x=CA1, y=CA2, fill=Treatment), alpha=0.1, size=3) +
  geom_text(data=cca.species, aes(x=CA1, y=CA2, label=microbial), size=2) +
  labs(x=paste("CA1 (",round(exp$CA1 * 100, digits=2), "%)"),
       y=paste("CA2 (",round(exp$CA2 * 100, digits=2), "%)"),
       title="Soil Microbial Community", subtitle="")+
  scale_color_manual(values=c("blue", "red", "green", "gray")) +
  scale_fill_manual(values=c("blue", "red", "green","gray"))
ggsave("Soil microbial community_CA.jpg", width=14, height=7)


# check if it is significant based on statistics (PERMANOVA)
adonis2(data.ca[, 2:ncol(data.ca)] ~ data.ca$Soil_Type)






############################### Canonical correspondence analysis (CCA)
rm(list=ls()) # Remove Previous variables


# Install packages
#install.packages("tidyr")
library(tidyr)
#install.packages("vegan")
library(vegan)
#install.packages("grid")
library(grid)
#install.packages("ggplot2")
library(ggplot2)


# load community matrix
Microbial_community <- read.csv("Microbial Community.csv", stringsAsFactors=FALSE)
str(Microbial_community) #Structure
Microbial_community[, c(2:ncol(Microbial_community))] <- sapply(Microbial_community[, c(2:ncol(Microbial_community))], as.numeric) # convert community matrix into numeric
Microbial_community[is.na(Microbial_community)] <- 0 # convert NA to 0 (It is necessary for CCA)



# Load environmental data
envr <- read.csv("Environmental Data.csv", stringsAsFactors=TRUE)
str(envr)
envr[is.na(envr)] <- 0 # convert NA to 0 (It is necessary for CCA)




#  select envr factors of interest in the Standardisation step
envr <- decostand(envr[,2:ncol(envr)], meth = "standardize")
?decostand # see the information about standardization method
pairs(envr) # check for collinearity 

#  create dataframes with and without grouping factor
Microbial_community.all <- Microbial_community # With grouping factor
Microbial_community <- Microbial_community [,2:ncol(Microbial_community)] # Without grouping factor



#Create function for CCA analysis
cca.fun <- function(Microbial_community, Microbial_community.all, envr){
  mod <- cca(X = Microbial_community, Y = envr) # create CCA list
  cca.sites <- data.frame(scores(mod, choices = c(1,2), display = "sites", scaling = 3)) # extract data for sites CCA (in CA1 and CA2)  
  ord_df <- data.frame(Treatment = Microbial_community.all$Soil_Type, CCA1 = cca.sites$CCA1, CCA2 = cca.sites$CCA2) # create dataframe with categorical variable and site scores(CA1 and CA2)
 ord_df$Treatment <- factor(ord_df$Treatment, levels = c("unfertilized","Chemical", "Organic" , "Lime")) # Convert Treatment variable into factor with suitable order
  exp <- summary(eigenvals(mod)) # Extract the proportion of variance explained by each CA axis
  exp <- data.frame(exp) # convert into dataframe
  exp <- subset(exp, rownames(exp)=="Proportion Explained")  # keep just Proportion Explained data
  cca.species <- data.frame(scores(mod, display = "species",scaling = 3)) # extract Species Scores from an Ordination
  cca.species <- data.frame(Cca1 = cca.species$CCA1, Cca2 = cca.species$CCA2, species = rownames(cca.species)) # add new column with row names
  cca.envr <- data.frame(scores(mod, display = "bp", scaling = 3)) # extract environmetal Scores from an Ordination
  cca.envr <- data.frame(cca1 = cca.envr$CCA1, cca2 = cca.envr$CCA2, factor = rownames(cca.envr)) # create new dataframe with row names and scores of Environmental data on CA1 and CA2
  res_list <- list(ord_df,cca.species,cca.envr,exp) # make list from previous dataframes
  return(res_list)
}




# Perform CCA function
ccares <- cca.fun(Microbial_community, Microbial_community.all , envr)

# plotit
ggplot(ccares[[1]]) +
  
  geom_point(mapping = aes(x = CCA1, y = CCA2, colour = Treatment), size = 2.5) +
  geom_text(data = ccares[[2]], 
            aes(x = Cca1, y = Cca2, label = species), size = 2.5) +
  geom_segment(data = ccares[[3]],
               aes(x = 0, xend = cca1, y = 0, yend = cca2),
               arrow = arrow(length = unit(0.5, "cm")), size = .1) +
  geom_text(data = ccares[[3]], 
            aes(x = cca1  , y = cca2 , label = factor),
            size = 3) +
  labs(title = NULL, x = paste("CCA1 (",round(ccares[[4]][1,1] * 100, digits = 2), "%)"),
       y = paste("CCA2 (", round(ccares[[4]][1,2] * 100, digits = 2), "%)")) +
  scale_color_manual(values=c("blue", "red", "green", "gray")) +
  theme_bw() 

ggsave("CCA_Soil_Microbial Community.jpg", width=20, height=12)

### Exercise:
# Do CA and CCA for enzyme data with the same environmental dataset



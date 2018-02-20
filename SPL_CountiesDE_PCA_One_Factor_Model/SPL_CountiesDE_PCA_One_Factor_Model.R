#------------------------------------------------------------------------
#Quantlet: Principal Component analysis with the reduced dataset
#------------------------------------------------------------------------


setwd("C:/Users/Christian/Google Drive/Master/Eigener_R_Projektordner")

dat = read.table("data.txt", header = TRUE, sep = "\t", 
    dec = ",", na.strings = "NA", check.names = FALSE)

dat[, 4:10] = lapply(dat[, 4:10], function(x) as.numeric(as.character(x)))

#Loading relevant packages
library ("psych")
library ("paran")
library ("lattice")


#--------------------------------------------------------------------------
# 1.1. DATA PREPARATION
#--------------------------------------------------------------------------

#Delete superficial Variables 
dat_pca1 = dat[,-c(1,2,3,10,11,12,13)]


#--------------------------------------------------------------------------
# 1.2How many principal components to be retained?
#--------------------------------------------------------------------------

#How many Principal Components need to be retained? - Scree Plot
scree(dat_pca1)
#Result: One component model

#How many Principal Components need to be retained? - Parallel-Analysis of Horn
paran(dat_pca1, centile=95, all=T, graph=T)

#--------------------------------------------------------------------------
# 1.3. Principal component analysis for the two-component solution
#--------------------------------------------------------------------------

#Conducting the PCA
pca1 = principal(dat_pca1, nfactors = 2, scores = TRUE, rotate = "none")
pca1

#--------------------------------------------------------------------------
#2.1. Data Preparation:
#Excluding the variable "GDP growth rate" and applying a one-component pca
#--------------------------------------------------------------------------

#Delete superficial Variables 
dat_pca2 = dat[,-c(1,2,3,7,10,11,12,13)]

#--------------------------------------------------------------------------
#2.2 How many principal components to be retained?
#--------------------------------------------------------------------------

#How many Principal Components need to be retained? - Scree Plot
scree(dat_pca2)
#Result: One component model

#How many Principal Components need to be retained? - Parallel-Analysis of Horn
paran(dat_pca2, centile=95, all=T, graph=T)

#--------------------------------------------------------------------------
# 2.3. Principal component analysis for the two-component solution
#--------------------------------------------------------------------------

#Conducting the PCA
pca2 = principal(dat_pca2, nfactors = 1, scores = TRUE, rotate = "none")
pca2


#---------------------------------------------------------------------------------
#3. Some inference: Can we explain a region's attractiveness by unemployment?
#---------------------------------------------------------------------------------

#Extracting scores of pca2
pca2_scores = pca2$scores
#pca2_scores

#Linking Scores to regions
dat_attract = data.frame(dat$Name, dat$Bundesland, pca2_scores)
View(dat_attract)

#Eyball analysis: Scatterplot of Attractivity vs. Unemployment
unemployment_z = scale(dat$Unemployment)
xyplot(pca2_scores ~ dat$Unemployment, pch = 3, cex = 0.5, 
       col = "black", main = "Scatterplot with linear regression line", 
       ylab = "Attractivity Scores", xlab = "Unemployment", type = c("p","r"))

#Running the simple linear regression and checking model assumptions
unem_lm = lm(pca2_scores ~ dat$Unemployment)
summary(unem_lm)
plot(unem_lm)


#Latex-Code for Table
#install.packages("stargazer")
library("stargazer")
stargazer(unem_lm, title="Regression Results")




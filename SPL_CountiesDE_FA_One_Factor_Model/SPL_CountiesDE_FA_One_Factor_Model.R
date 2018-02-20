#----------------------------------------------------------------------------
#Quantlet: Factor Analysis
#----------------------------------------------------------------------------

#setwd("C:/..")#set a working directory

dat = read.table("data.txt", header = TRUE, sep = "\t", 
    dec = ",", na.strings = "NA", check.names = FALSE)

dat[, 4:10] = lapply(dat[, 4:10], function(x) as.numeric(as.character(x)))

#Loading relevant packages
library ("psych")
library ("paran")
library ("lattice")


#--------------------------------------------------------------------------
# 1.) DATA PREPARATION
#--------------------------------------------------------------------------

#Delete superficial Variables which are either not numerical or not part of the original dataset.
dat_pca1 = dat[,-c(1,2,3,10,11,12,13)]

#---------------------------------------------------------------------------
# 2.) IS THE DATA SUITED FOR PCA/FA?
#---------------------------------------------------------------------------

#Matrix-Plot
p = solve(cor(dat_pca1, use="complete.obs"))
matplot(p, show.legend=T, axes=F)

#Bartlett-Test of Sphericity
cortest.bartlett(dat_pca1)

#Main indicator for suitability: KMO/MSA
KMO(dat_pca1)

#---------------------------------------------------------------------------
#3.) Limitation of the dataset: Exclusion of "GDP growth rate"
#---------------------------------------------------------------------------

dat_pca2 = dat[,-c(1,2,3,7,10,11,12,13)]


#---------------------------------------------------------------------------
#4.) HOW MANY PRINCIPAL COMPONENTS SHALL BE RETAINED?
#---------------------------------------------------------------------------

#Parallel-Analysis of Horn
paran(dat_pca2, centile = 95, all = T, graph = T)
# Result: retaining 2 components.

#Ellbow criterion
scree(dat_pca2)

#----------------------------------------------------------------------------
#5.) Exploratory Factor Analysis 
#----------------------------------------------------------------------------

# FA with principal component extraction
fa1 = principal(dat_pca2, nfactors = 1, rotate = "none")
fa1

# FA with axis extraction
fa2 = fa(dat_pca2, nfactors = 1, rotate = "none", fm = "pa")
fa2

# FA with maximum likelihood extraction
fa3 = fa(dat_pca2, nfactors = 1, rotate = "none", fm = "ml")
fa3

#FA with least squares extraction
fa4 = fa(dat_pca2, nfactors = 1, rotate = "none")
fa4

#Checking the Tucker Index of factor congruence
factor.congruence(fa1,fa2)
factor.congruence(fa2,fa3)
factor.congruence(fa3,fa4)
factor.congruence(fa1,fa3)
factor.congruence(fa1,fa4)
factor.congruence(fa2,fa4)

#---------------------------------------------------------------------
#6) RELIABILITY OF THE ONE FACTOR MODEL
#---------------------------------------------------------------------

#Evaulating Cronbach's Alpha
dat_pca3 = na.omit(dat_pca2)
fa       = fa(dat_pca3)
vars     = abs(fa$loadings)>0.5
alpha(cor(dat_pca3[,vars]), check.keys = T)


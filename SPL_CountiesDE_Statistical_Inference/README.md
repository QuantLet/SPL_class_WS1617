[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_CountiesDE_Statistical_Inference** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: SPL_CountiesDE_Statistical_Inference

Published in: 'Statistical Programming Languages - Student Project on ''How attractive are German counties and county-level cities? A statistical analysis of various economic factors in terms of attractiveness and welfare'' '

Description: Runs various statistical tests and a linear regression

Keywords: tests, distribution, mean comparison, variance homogeneity, qqplot, histogram, linear regression

Author: Jan Witzel

See also: other Quantlets in this project

Submitted: 14.03.2017

Datafile: no

Input: data.txt

```

### R Code
```r

# ======================================================
# ================== Data Preparation ==================
# ======================================================

dat = read.table("data.txt", header = TRUE, sep = "\t", dec = ",", 
                 na.strings = "NA", check.names = FALSE)
dat[,4:10] = lapply(dat[,4:10], function(x) 
  as.numeric(as.character(x)))

# load packages
library(nortest)		#normality tests
library(tables)			#latex output

# variables for different Categories can be listed as follows:
DatCounties = dat$Category == "County"
#set apart Net Migration
NetmigCount = dat[DatCounties,]$`Net Migration`
NetmigCouLC = dat[!DatCounties,]$`Net Migration`
#set apart GDP growth rate
GDPgrCount  = dat[DatCounties,]$`GDP growth rate`
GDPgrCouLC  = dat[!DatCounties,]$`GDP growth rate`
#set apart Unemployment
UnempCount  = dat[DatCounties,]$Unemployment
UnempCouLC  = dat[!DatCounties,]$Unemployment

#===================================================================
#===================================================================
#graphics to evaluate normality distribution for selective variables
#===================================================================
#===================================================================

#devide variables into categories
NetmigCount = subset(dat$`Net Migration`, dat$Category == "County")
NetmigCLC = subset(dat$`Net Migration`, dat$Category == "County-Level City")
GDPgrCount = subset(dat$`GDP growth rate`, dat$Category == "County")
GDPgrCLC = subset(dat$`GDP growth rate`, dat$Category == "County-Level City")

#twoXtwo plots
par(mfrow = c(2,2))
#1
#Histogram
#Net Migration County
hist(NetmigCount, freq = FALSE, breaks = 20, ylab = "", 
     xlab = "Net Migration County", xlim = c(-2, 2), cex.main = 1.2, 
     main = "Histogram vs Normal Distribution")
#plotting curve and histogram
x = seq(-10, 10, 0.01)
curve(dnorm(x, mean = mean(NetmigCount),
            sd = sd(NetmigCount)), add = TRUE)

#QQPlot
#Net Migration County
qqnorm(NetmigCount, pch = 16, col = "Red", cex.main = 1.2,  
       main = "Normal Q-Q Plot", ylab = "",
       xlab = "Net Migration County")
qqline(NetmigCount)

#2
#Histogram
#Net Migration County-Level City
hist(NetmigCLC, freq = FALSE, breaks = 20, cex.main = 1.2, 
     xlab = "Net Migration County-Level City", xlim = c(-2, 2),  
     main = "Histogram vs Normal Distribution", ylab = "")
x = seq(-10, 10, 0.01)
curve(dnorm(x, mean = mean(NetmigCLC),
            sd = sd(NetmigCLC)), add = TRUE)
#QQPlot
#Net Migration County-Level City
qqnorm(NetmigCLC, pch = 16, col = "Red", cex.main = 1.2,
       main = "Normal Q-Q Plot", ylab = "", 
       xlab = "Net Migration County-Level City")
qqline(NetmigCLC)

#3
#Histogram
#GDP growth rate County
hist(GDPgrCount, freq = FALSE, breaks = 20, cex.main = 1.2,
     xlab = "GDP growth rate County", ylab = "", xlim = c(-2, 8), 
     main = "Histogram vs Normal Distribution")
#plotting curve and histogram
x = seq(-10, 10, 0.01)
curve(dnorm(x, mean = mean(GDPgrCount),
            sd = sd(GDPgrCount)), add = TRUE)
#QQPlot
#GDP growth rate County
qqnorm(GDPgrCount, pch = 16, col = "Red", cex.main = 1.2,
       main = "Normal Q-Q Plot", ylab = "", 
       xlab = "GDP growth rate County")
qqline(GDPgrCount)

#4
#Histogram
#GDP growth rate County-Level City
hist(GDPgrCLC, freq = FALSE, breaks = 20, cex.main = 1.2, 
     ylab = "", xlim = c(-4,8),
     xlab = "GDP growth rate County-Level City", 
     main = "Histogram vs Normal Distribution")
#plotting curve in histogram
x = seq(-10, 10, 0.01)
curve(dnorm(x, mean = mean(GDPgrCLC),
            sd = sd(GDPgrCLC)), add = TRUE)

#QQPlot
#GDP growth rate County
qqnorm(GDPgrCLC, pch = 16, col = "Red", cex.main = 1.2, 
       main = "Normal Q-Q plot", ylab = "", 
       xlab = "GDP growth rate County-Level City")
qqline(GDPgrCLC)



# ======================================================
# =============== Regression Model =====================
# ======================================================

# package for variance inflation factor
install.packages(car)
library(car)

# package to create Latex code
install.packages("stargazer")
library(stargazer)

#=========model log unemployment========================

# create and add new variable
logunempl     = log(dat$Unemployment)
dat$logunempl = logunempl

pairs(dat[c(6, 7, 9, 14)])			# create scatterplot

# estimate model
model = lm(dat$logunempl~dat$`GDP growth rate`+
             dat$`Workers in Agriculture`+dat$`Net Migration`)
summary(model)

# check for multicollinearity
vif(model)

# Output to Latex
stargazer(model, title = "Regression Results")

# ======================================================
# =============== Statistical Inferences ===============
# ======================================================


#=======comparison of two means of selected variables==
#======================================================
# Conditions for t-test:===============================
# normal distribution================================== 
# variance homogenity==================================
# independence of variables============================

#===========test for normal distribution===============
#test for normality using various tests
normalitySW  = apply(dat[,4:13], MARGIN = 2, FUN = shapiro.test)
normalityAD  = apply(dat[,4:13], MARGIN = 2, FUN = ad.test)
normalityCVM = apply(dat[,4:13], MARGIN = 2, FUN = cvm.test)
normalityL   = apply(dat[,4:13], MARGIN = 2, FUN = lillie.test)

#skewness and kurtosis
apply(dat[,4:13], MARGIN = 2, FUN = skewness)
apply(dat[,4:13], MARGIN = 2, FUN = kurtosis)

#result: none of the variables are distributed normally

#========test for variance homogenity==================

install.packages("car")
library(car)
leveneTest(dat$`Net Migration`, dat$Category)
leveneTest(dat$`GDP growth rate`, dat$Category)


#===========mean comparison============================

#t-test
t.test(dat$`Net Migration` ~ dat$Category, var.equal = TRUE)
t.test(dat$`GDP growth rate` ~ dat$Category, var.eqaul = TRUE)

#all variable's means differ significantly
```

automatically created on 2018-05-28
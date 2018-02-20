# Clean up the workspace
rm(list = ls())
if(length(sessionInfo()$otherPkgs) > 0)
    lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
        character.only = TRUE, unload = TRUE, force = TRUE)

# Load libraries
library("caret") 
library("randomForest") 
library("pROC") 
library("caretEnsemble") 
library("ROSE") 



# Set a seed
set.seed(123)

# Load the full prepared data set
dat = read.csv("dat4.csv")

# Format to date
dat$date = as.Date(dat$date, format = "%d-%b-%y")

# reducing dataset to democrats and republican only
dat = subset(dat, party == "R" | party == "D")

# undersampling for use with randomForest package: (undersampling in caret
# is defined in trainControl()) function is part of the ROSE package, 
# undersamples the specified dataset such that the specified variable is equally 
# distributed in the new dataset 
# method parameters are "over", "under" and "both"

dat_unders = ovun.sample(party ~ ., data = dat, method = "under")$data

# splitting dataset into train and test for processing with caret this allows us 
# to train a random forest on one part of the dataset and then use the model 
# trained in this way to predict cases in the remaining part

idx.train = createDataPartition(y = dat$party, p = 0.8, list = FALSE) 
train     = dat[idx.train,] 
test      = dat[-idx.train,]

# random forest with default parameters with randomForest package for partial
# dependence and variable importance
# the randomForest packagage allows us to gain more insight into the effects of 
# the predictor variables on the response variable. These functions are not as 
# extensive in the caret package, which is why whe chose to build our random 
# forest model for variable importance with the caret package

partial.rf = randomForest(party ~ amount + gender + class + distr + date + 
    area2_party, data = dat_unders) 


# generate a partial dependence plot using the randomForest object and save plot 
# coordinates 
x = partialPlot(partial.rf, pred.data = dat_unders, which.class = 'R',
    x.var = "amount", main = "Partial Dependence on gender")
y = partialPlot(partial.rf, pred.data = dat_unders, which.class = 'R',
    x.var = "distr", main = "Partial Dependence on amount")

# replot partial dependence plot with ggplot2
# first the variables are saved as a dataframe
x    = as.data.frame(x)
x$x  = as.vector(x$x)
x$y  = as.vector(x$y)

# Plot it
pdp1 = ggplot(x, aes(x, y, color = "firebrick"), size = 2) +
    geom_line() +
    ggtitle("Partial Dependence on Donation Amount") + 
    xlab("Donation Amount") + 
    ylab("") + 
    guides(colour = FALSE) + 
    theme_bw(base_size = 10)
    
# replot partial dependence plot with ggplot
y    = as.data.frame(y)
y$x  = as.vector(y$x)
y$y  = as.vector(y$y)

pdp2 = ggplot(y, aes(x, y, color = "chocolate"), size = 2) +
    geom_line() +
    ggtitle("Partial Dependence on Wealth Distribution") + 
    xlab("Wealth Distribution") + 
    ylab("") + 
    guides(colour = FALSE) + 
    theme_bw(base_size = 10)

ggsave("FECAvariableimportance_1.png", pdp1)
ggsave("FECAvariableimportance_2.png", pdp2)

# extract variable importance data and replot with ggplot measures mean decrease
# in gini for the variables used is measured by permuting each of the varibales 
# and then computing the mean decrease in gini over the variables used
a = varImpPlot(partial.rf)
a = as.data.frame(a)

# put it in a data.frame
df  = data.frame(variable = c(rownames(a)), MDG = a$MeanDecreaseGini)

# using transform() the variables in the dataset are ordered by their mean decrease
# in gini, so that the bars in the plot are ordered
df = transform(df, variable = reorder(variable, MDG))

# plot is replotted as a barchart using ggplot
barchart = ggplot(df, aes(variable, MDG)) + 
    geom_bar(stat = "identity", fill = c("darkolivegreen","darkorange", 
        "firebrick","chocolate","darkcyan","goldenrod")) +  
    ggtitle("Mean Decrease in Gini") +
    xlab("") + 
    ylab("") + 
    theme_bw(base_size = 10) + 
    coord_flip()

ggsave("FECAvariableimportance_3.png", barchart)

[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)
## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SPL_FECArandomforest** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)
```yaml
Name of Quantlet: SPL_FECArandomforest

Published in:     Statistical Programming Languages

Description:      'Predictive model of donation behaviour. Tries to predict the party donated to 
                  based on donor characteristics. Also attemps to visualize effect of predictor 
                  variables.'

Keywords:         fec, election, randomforest, machine-learning, prediction

Author:           Janek Willeke

Submitted:        Mon, March 13 2017

Datafile:         dat4.csv
```

## R Code
```r
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

# random forest. method is "repeated cv", with number = 10 and repeats = 5 while
# downsampling in the process summaryfunction returns auc, precision, accuracy


model.control     = trainControl(
    method          = "repeatedcv", # cross validation
    number          = 5, # number of folds in cross validation 
    repeats         = 2,  # number of repeats                   
    classProbs      = TRUE, # Return class probabilities
    summaryFunction = twoClassSummary,
    sampling        = "down", # returns auc, precision, accuracy
    set.seed(194))



# Definition of a search grid of values to test for a sequence of randomly
# sampled variables as candidates at each split (mtry)
# rule-of thumb for the optimal value is the square root of the variables,
# should be set to a vector ranging around this value, 3:4 in this case
rf.parms = expand.grid(mtry = c(3:4))  


# Train random forest ("rf") using a 5-fold repeated cross validation 
# with 2 repeats and 500 trees

# the train() function in the caret package can be used for a number of 
# algorithms. One of the advantages of caret is that there is only one interface 
# for an array of different functions

rf.caret = train(party~ area + gender + class + distr, 
    data       = train, dataset used, here: the train data defined above
    method     = "rf", # method used, random forest in this case
    ntree      = 500, # specifies number of trees grown
    tuneGrid   = rf.parms, # the tune grid used, see above
    metric     = "ROC", # specifies the metric for model performance 
    trControl  = model.control # the model.control parameters defined before
    )
                  

# Predict the party donated to as class
yhat.rf.caret   = predict(rf.caret, newdata = test, type = "raw")  
# prediction of the test set, type= "raw" sets predictions to class predictions, 
# "probability" to class probabilities. rf.caret specifies the model used, 
# newdata the data to predict

# confusionMatrix shows the amount of correctly and falsely classified cases                           
conm = confusionMatrix(yhat.rf.caret, reference = test$party) 
```

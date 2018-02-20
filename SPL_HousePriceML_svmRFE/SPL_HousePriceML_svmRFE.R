########################### SMV feature selection #########################
rm(list = ls())
graphics.off()

libraries = c("caret", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

load("basic_processing.RData")

train = basic_data$train
y = train$y
train$y = NULL

k = 5  # folds on the cv loop

subsets = 30:99 # subset to be evaluated

# specify model parameters
svmGaussianGrid = expand.grid(C = 4.5, sigma = 0.002)
# specify rfe options
ctrl = rfeControl(functions = caretFuncs, method = "cv", number = k, verbose = TRUE)
# perform actual varaible selection
rfe_gaussianSVM = rfe(x = train, y = y, sizes = subsets, rfeControl = ctrl, method = "svmRadial", 
    metric = "RMSE", tuneGrid = svmGaussianGrid)
# get varaible ranking and plot it
feature_ranking = predictors(rfe_gaussianSVM)
ggplot(rfe_gaussianSVM, type = c("g", "o"))

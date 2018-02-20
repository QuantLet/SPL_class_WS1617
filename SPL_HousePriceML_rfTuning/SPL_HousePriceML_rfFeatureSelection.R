############################ Tuning Random Forrest using H20 ###################################

### setwd('F:/PHD/IRTG/courses/SPL/Quantnet/rfTuning')
rm(list = ls())
graphics.off()
######### if package 'h2o' is not installed before, run the following code ############
######### source('install_h2o.R')

library(h2o)
load("basic_processing.RData")

## Create an H2O cloud (invoke java virtual machine)
h2o.init(nthreads = -1, max_mem_size = "2G")
h2o.removeAll()  # Clean slate - just in case the cluster was already running

# get preprocessed training set
train = basic_data$train
test = basic_data$test

# specify repetions and number of folds
repetitions = 2
k_folds = 5

# convert into h20_objects
train_h2o = as.h2o(train)
# sepecify columns of inputs and labels
col_label = which(colnames(train) == "y")
col_input = which(colnames(train) != "y")

# specify grid
rfGrid = list(mtries = seq(10, 30, 5), ntrees = 500, max_depth = seq(5, 25, 5), sample_rate = seq(0.6, 
    0.8, 0.2), stopping_metric = "MSE", stopping_rounds = 20)
# draw new seeds in order to run a different cv split each repetition
seeds = sample(1:1000, repetitions)

# define variables to store outcomes
rfFit = list()
tuning_results = list()
for (t in 1:repetitions) {
    # tune random forest with h2o
    rfFit[[t]] = h2o.grid("randomForest", grid_id = "gridSearch", x = col_input, y = col_label, 
    training_frame = train_h2o, hyper_params = rfGrid, nfolds = k_folds, is_supervised = TRUE, 
    seed = seeds[1])
    # get tuning results and save them as csv
    tuning_results[[t]] = h2o.getGrid(grid_id = "gridSearch", sort_by = "rmse")
}

h2o.shutdown()

### save results
res_save = function(x, seed) {
    aa = x@summary_table
    path = paste('output/tuning_result_seed_', seed, '.csv', sep = '')
    write.csv(aa, path, row.names = F)
}
mapply(res_save, tuning_results, seeds)

### show some plots 
#TODO: add axis labels of the plot!!!
par(mfrow = c(1, 2))
plotRMSE = function(x, seed) {
    aa = x@summary_table$rmse
    index = seq_along(aa)
    min_aa = min(aa)
    index_min = index[aa == min(aa)][1]
    plot(index, aa, main = paste0("RMSE when seed = ", seed), xlab = "parameter set", ylab = "RMSE", pch = 20, 
        col = "grey50")
    points(index_min, min_aa, pch = 20, col = "red")
}
mapply(plotRMSE, tuning_results, seeds)
### the parameter set that is choosen is: max_depth = 25, mtries = 25, ntrees = 500, sample_rate
### = 0.8.
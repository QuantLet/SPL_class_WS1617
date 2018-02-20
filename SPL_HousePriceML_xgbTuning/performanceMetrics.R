#################### Performance Metrics ##############################

# calculates normal mse input: y - actual labels of the validation set yhat - predicted label
# by the model --> output of predict(modelFit, newdata = validation)) output - Mean Squared
# Error
mse = function(y, yhat) {
    return(sum(y - yhat)^2/length(y))
}

# calculates normal rmse input: y - actual labels of the validation set yhat - predicted label
# by the model --> output of predict(modelFit, newdata = validation)) output - Root Mean
# Squared Error
rmse = function(y, yhat) {
    return(sqrt(mse(y, yhat)))
}

# calculates rmse of the log data (Kaggle evaluation measure) input: y - actual labels of the
# validation set yhat - predicted label by the model --> output of predict(modelFit, newdata =
# validation)) output - Root Mean Squared Error of log(y) and log(yhat) --> comparable to
# Kaggle Leaderboard Scores!
rmse_log = function(y, yhat) {
    return(rmse(log(y), log(yhat)))
}


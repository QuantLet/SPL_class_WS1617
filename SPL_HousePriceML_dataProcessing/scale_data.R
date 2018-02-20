#################### scaling data for regression model and pca ################

# this function performs min(0) max(1) scaling meaning that all numeric are scale between 0
# and 1 note: scaling will be bad if no outlier detection was performed previously input: a
# variable output: a scale version of the variable in case it is numerical
min_max_scaling = function(x) {
    if (is.numeric(x)) {
        return((x - min(x))/(max(x) - min(x)))
    } else {
        return(x)
    }
}

# this function normalizes the data i.e. x - mu/sigma note: scaling will be bad if no outlier
# detection was performed previously input: a variable output: a scale version of the variable
# in case it is numerical
gaussian_scaling = function(x) {
    if (is.numeric(x)) {
        return(scale(x, center = TRUE, scale = TRUE))
    } else {
        return(x)
    }
}
# this function scales the all numerical variable in the feature matrix input: X-no_outliers -
# feature matrix without NAs and outliers scale_method - a string that determines the scaling
# method, if 'min_max' then min_max_scaling function is applied else gaussian_scaling is
# applied output: scale feature matrix
scale_data = function(X_no_outliers, scale_method) {
    if (scale_method == "min_max") {
        return(as.data.frame(lapply(X_no_outliers, min_max_scaling))[, -1])  # without id variable
    }
    if (scale_method == "gaussian") {
        return(as.data.frame(lapply(X_no_outliers, gaussian_scaling))[, -1])
    }
}


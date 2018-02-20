################### delete near zero variance variable #######################
library(caret)

delect_nz_variable = function(X) {
    near_zero_variance = nearZeroVar(X)
    return(X[, -near_zero_variance])
}

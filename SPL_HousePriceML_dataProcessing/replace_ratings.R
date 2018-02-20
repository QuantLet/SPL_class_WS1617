########################################## Feature Engineering #####################################################

libraries = "plyr"
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# replace NA with None if NA mean that there is None
replace_NA_zero = function(x) {
    x = factor(x, levels = c(levels(x), 0))
    x[is.na(x)] = 0
    return(x)
}


# replace variable with the standard rating scheme i.e. Ex,Gd,TA,Fa,Po with numeric ratings
replace_standard_ratings = function(x) {
    x = replace_NA_zero(x)
    x = revalue(x, c(Ex = 5, Gd = 4, TA = 3, Fa = 2, Po = 1))
    return(x)
}

# replace basement ratings with numerical ratings
unifinish_dummy = function(X_com) {
    X_com$BsmtFinType1_unfinished = ifelse(X_com$BsmtFinType1 == "Unf", 1, 0)
    X_com$BsmtFinType1_unfinished[is.na(X_com$BsmtFinType1_unfinished)] = 0
    X_com$BsmtFinType2_unfinished = ifelse(X_com$BsmtFinType2 == "Unf", 1, 0)
    X_com$BsmtFinType2_unfinished[is.na(X_com$BsmtFinType2_unfinished)] = 0
    return(X_com)
}

# function to replace ratings for Bsmt..
replace_base_ratings = function(x) {
    x = replace_NA_zero(x)
    x = revalue(x, c(GLQ = 5, ALQ = 4, BLQ = 3, Rec = 2, LwQ = 1, Unf = NA))
    return(x)
}

# function to replace all ratings that are given in string into numeric rating scores
replace_ratings = function(X_com) {
    # replace standard rating schema
    name_standard_rating = c("PoolQC", "GarageCond", "GarageQual", "FireplaceQu", "KitchenQual", 
        "HeatingQC", "BsmtCond", "BsmtQual", "ExterCond", "ExterQual")
    idx_standard = which(colnames(X_com) %in% name_standard_rating)
    X_replace = X_com[, idx_standard]
    X_replaced = data.frame(lapply(X_replace, replace_standard_ratings))
    X_com = cbind(X_com[, -c(idx_standard)], X_replaced)
    
    # replace basement schema (Bsmt...)
    X_com = unifinish_dummy(X_com)
    name_base_rating = c("BsmtFinType1", "BsmtFinType2")
    idx_base = which(colnames(X_com) %in% name_base_rating)
    X_replace = X_com[, idx_base]
    X_replaced = data.frame(lapply(X_replace, replace_base_ratings))
    X_com = cbind(X_com[, -c(idx_base)], X_replaced)
    
    # replace NA in variable where they actuale mean none instead of na, we replace value with 0
    name_na_means_none = c("MiscFeature", "Fence", "GarageFinish", "Alley")
    idx_na_means_none = which(colnames(X_com) %in% name_na_means_none)
    X_replace = X_com[, idx_na_means_none]
    X_replaced = data.frame(lapply(X_replace, replace_NA_zero))
    X_com = cbind(X_com[, -c(idx_na_means_none)], X_replaced)
    
    # replace ratings in BsmtExposure
    X_com$BsmtExposure = revalue(X_com$BsmtExposure, c(Gd = 3, Av = 2, Mn = 1, No = 0))
    return(X_com)
}

######################## Function to convert a categorical variable into dummies ######################## It also
######################## filters out dummies that are linear dependent to other according to the vif score to avoid
######################## multicolinearity assumes that you have installed the following libraries
libraries = c("dummies", "usdm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

cat_to_dummy = function(x, vif_threshold = 10) {
    # check if variable is not numeric or has less than 5 levels else convert
    if (!any(is.numeric(x)) | length(unique(x)) <= 4) {
        # convert into dummy and exclude randomly drawn index
        x = as.data.frame(dummy(x))
        # get number of column after hot encoding
        initial_colnumber = ncol(x)
        # check if more than one dummy remains
        if (ncol(x) > 2) {
            # drop dummy that are linear dependent with other variables exclude from the usdm package
            # delets all dummies with a higher score than the vif_threshold
            x = exclude(x, vifstep(x, th = vif_threshold))
            # if no column was delete we delete one randomnly to avoid dummy trap
            if (ncol(x) == initial_colnumber) {
                random_drop = sample(1:length(unique(x)), 1)
                x = x[, -random_drop]
            }
        }
        if (ncol(x) == 2) {
            x = x[, -1]
        }
    }
    return(x)
}


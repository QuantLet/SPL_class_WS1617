######################### impout outlier with upper and lower bounds #####################3
iqr_outlier = function(x) {
    if (is.numeric(x)) {
        bp = boxplot(x)
        lower = bp$stats[1]
        upper = bp$stats[5]
        x[x > upper] = upper
        x[x < lower] = lower
    }
    return(x)
}
# usage X_no_outlier = as.data.frame(lapply(X_imputed, iqr_outlier))

z_outlier = function(x) {
    if (is.numeric(x)) {
        z = scale(x, center = TRUE, scale = TRUE)
        if (abs(z) > 3) {
            
        }
    }
}

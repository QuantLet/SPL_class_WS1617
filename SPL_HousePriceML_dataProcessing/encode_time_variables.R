########################## Encode Time Variables #######################

include_quarter_dummies = function(X_com) {
    X_com$SoldSecondQuartal = ifelse(X_com$MoSold %in% 4:6, 1, 0)
    X_com$SoldThirdQuartal = ifelse(X_com$MoSold %in% 7:9, 1, 0)
    X_com$SoldFourthQuartal = ifelse(X_com$MoSold %in% 10:12, 1, 0)
    X_com$MoSold = NULL
    return(X_com)
}


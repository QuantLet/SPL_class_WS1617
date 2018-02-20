# ------------------------------------------------------------------------------
# Quantlet No. 1 : Data Preparation Quantlet
#
# This quantlet creates a function to automatically provide historical stock 
# prize data for the VaR and CVaR analysis. After downloading time series data 
# for the required stocks from the internet, they are converted into data frame 
# and all occurring NA observations are imputed. Currency effects are adjusted 
# for american stocks. Finally, the returns of single stocks and those of a 
# hypothetical portfolio are calculated.
# ------------------------------------------------------------------------------

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("tseries", "zoo")
lapply(libraries, function(x) 
  if (!(x %in% installed.packages())) {
    install.packages(x) 
  }
)
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# -----------------------------------------------------------------------------
# Setting of parameters 
# -----------------------------------------------------------------------------

start.date = "2013-01-01"
end.date   = "2016-10-31"
quote.type = "AdjClose"

# define used stocks and weights
stocks  = c("dbk.de", "mcd", "^gdaxi", "gs", "pfe", "nke")
weights = c(0.2, 0.3, 0.1, 0.15, 0.15, 0.1) # must have same length as stocks

# -----------------------------------------------------------------------------
# Calculation function for weighted portfolio returns
# -----------------------------------------------------------------------------

pf_returns = function(stocks, start_weights, start, end, quote) {
  
  # check if weights sum up to 1
  if (sum(start_weights) != 1) {
    stop("Error: Specified weights have to sum up to 1")
  }
  # check if every stock has exactly one weight
  else if (length(stocks) != length(start_weights)) {
    stop("Error: Vectors 'stocks' and 'start_weights' must have same length")
  } else {
    
    # -----------------------------------------------------------------------------
    # Download time series data for stock prizes from Yahoo Finance
    # -----------------------------------------------------------------------------
    
    # download time series data for stock prizes from Yahoo Finance    
    stock_pr = lapply(lapply(stocks, function(stocks) get.hist.quote(instrument = 
                      stocks, start = start, end = end, quote = quote)), 
                      function(x) cbind(x))
    
    # -----------------------------------------------------------------------------
    # Conversion of time series into data frame
    # -----------------------------------------------------------------------------
    
    # vector with lengths of single time series
    stock_val_prep = vector(length = length(stock_pr)) 
    stock_val_prep = as.numeric(as.vector(lapply(stock_pr, function(x) 
                     as.numeric(length(x)))))
    
    # convert sublists to data frame
    a = lapply(stock_pr, as.data.frame)
    a = lapply(a, function(x) cbind(rownames(x), x)) 
    
    # find time series with maximal number of values
    stock_val        = data.frame(a[[which.max(stock_val_prep)]])
    names(stock_val) = c("rownames(x)", "AdjClose")
    c                = (1:length(stock_pr))[-which.max(stock_val_prep)]
    
    # merge time series to data frame of numeric columns
    for (i in 1:length(c)) {
      stock_val = merge(stock_val, a[[c[i]]], by = "rownames(x)", all.x = TRUE)      
    }
    colnames(stock_val) = c("Date", stocks[which.max(stock_val_prep)], stocks[c])
    
    # -----------------------------------------------------------------------------
    # Imputation of NA values
    # -----------------------------------------------------------------------------
    
    cleaned_stock_val = cbind(stock_val[1], na.approx(stock_val[, -1]))
    cleaned_stock_val = cleaned_stock_val[-1, ]
    
    # -----------------------------------------------------------------------------
    # Cleaning of currency effects
    # -----------------------------------------------------------------------------
    
    # download time series for currency exchange prizes from Oanda
    FX       = get.hist.quote(instrument = "USD/EUR", start = start.date, 
                              end = end.date, provider = "oanda")
    FX       = data.frame(Date = as.factor(index(FX)), USDEUR = as.numeric(FX))
    currency = merge(cleaned_stock_val, FX, by = "Date", all.x = TRUE)  
    
    # function to determine ending substrings to indicate german stocks with suffix ".de"
    substrRight = function(x, n) {
      substr(x, nchar(x) - n + 1, nchar(x))
    }
    
    index_de = c("^gdaxi", "^stoxx50e", "^100e")
    cld_val  = cleaned_stock_val[, -1]
    
    # recalculate non-german stock (via ending ".de") prize with currency course 
    us_stocks  = cld_val[, which(substrRight(colnames(cld_val),3) != ".de" & 
                         !(colnames(cld_val) %in% index_de)), drop = FALSE] * 
                         currency$USDEUR
    all_stocks = cbind(cld_val[, which(substrRight(colnames(cld_val),3) == ".de"), 
                       drop = FALSE], cld_val[, colnames(cld_val) %in% index_de, 
                       drop = FALSE], us_stocks)
    
    # rename and reorder columns as defined in stock vector
    rownames(all_stocks) = cleaned_stock_val[, 1]
    stock_data = all_stocks[, stocks]
    
    # create tables on an anual basis
    nyears = as.numeric(difftime(end.date, start.date, units = "weeks")) / 52.17857
    years  = vector(length = ceiling(nyears))
    years  = sapply(1:length(years), function(x) as.numeric(format(as.Date(start.date, 
                    format = "%Y-%m-%d"), "%Y")) + (x - 1))
    
    stock_val_year = list(length = ceiling(nyears))
    stock_ret_year = list(length = nyears)
    
    for (i in 1:ceiling(nyears)) {
      stock_val_year[[i]]           = stock_data[grep(years[i], rownames(stock_data)), ]
      stock_ret_year[[i]]           = as.data.frame(sapply(log(stock_val_year[[i]]), diff))
      colnames(stock_val_year[[i]]) = stocks
      colnames(stock_ret_year[[i]]) = stocks
    }
    
    # -----------------------------------------------------------------------------
    # Calculation of log-returns, portfolio returns and normalized portfolio returns
    # -----------------------------------------------------------------------------
    
    # calculate log-returns for single stocks
    stock_ret_disc           = sapply(stock_data, function(x) diff(x) / x[-length(x)])
    stock_ret_cont           = as.matrix(log(1 + stock_ret_disc))
    rownames(stock_ret_disc) = cleaned_stock_val[-1, 1]
    rownames(stock_ret_cont) = cleaned_stock_val[-1, 1]  
    
    # calculate moving weights without rebalancing
    weights   = matrix(data = start_weights, nrow = dim(stock_ret_disc)[1], 
                       ncol = dim(stock_ret_disc)[2], byrow = TRUE)
    weights_m = matrix(data = start_weights, nrow = dim(stock_ret_disc)[1], 
                       ncol = dim(stock_ret_disc)[2], byrow = TRUE)
    
    for (i in 1:(nrow(stock_ret_disc) - 1)) {
      # calculate moving weights in period i + 1
      weights_m[i + 1, ] = t(weights[i, ]) * (1 + stock_ret_disc[i, ])
      
      # norm weights to sum 1
      weights[i + 1, ]   = weights_m[i + 1, ] / sum(weights_m[i + 1, ])
    }
    
    colnames(weights) = stocks    
    
    # calculate weighted portfolio log-returns
    stock_pf_ret = vector()
    
    for (i in 1:nrow(weights)) {
      stock_pf_ret[i] = log(1 + t(stock_ret_disc[i, ]) %*% as.matrix(weights[i, ]))
    }
    
    stock_pf_ret           = as.data.frame(stock_pf_ret)
    rownames(stock_pf_ret) = rownames(stock_data[-1, ])
    colnames(stock_pf_ret) = c("Returns")
    
    # normalizing function
    normalize = function(x) {
      mu     = mean(x)
      std    = sd(x)
      result = (x - mu) / std
      return(result)
    }
    
    # calculate normalized weighted portfolio returns
    stock_pf_norm_ret           = as.data.frame(lapply(stock_pf_ret, normalize))
    rownames(stock_pf_norm_ret) = rownames(stock_data[-1, ])
    colnames(stock_pf_norm_ret) = c("Normalized_Returns")
    
    # -----------------------------------------------------------------------------
    # Definition of list output
    # -----------------------------------------------------------------------------
    
    r        = list(stocks, weights, stock_ret_cont, stock_pf_ret, 
                    stock_pf_norm_ret, stock_ret_year)
    names(r) = c("Used_Stocks", "Weights", "Stock_Returns", "Returns", 
                 "Normalized_Returns", "Stock_Return_by_Year")
    return(r)
  }
}

stock_data = pf_returns(stocks, weights, start.date, end.date, quote.type)

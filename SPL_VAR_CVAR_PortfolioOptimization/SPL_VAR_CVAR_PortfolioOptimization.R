# -----------------------------------------------------------------------------
# Quantlet No. 4 : Portfolio Optimization Quantlet
#
# This quantlet calculates the efficient frontier of risky assets incl. 
# a risk free asset and a Value-at-Risk (VaR) or Conditional Value-at-
# Risk (CVaR) constraint. This model assumes an underlying normal 
# distribution. Further information can be found in Termpaper and its 
# sources
# -----------------------------------------------------------------------------

# ---------------------- 1. Script --------------------------------------------
# 1.1 Preparation -------------------------------------------------------------

# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("ggplot2", "tseries", "corrplot", "reshape2", "chron")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# ------------------------ 2. Data --------------------------------------------
# 2.1 Extraction --------------------------------------------------------------

source('Quantlet1/Data_Preparation_Quantlet.R') 

# ----------------------- 3. Parameter ----------------------------------------
# 3.1 Fixation ----------------------------------------------------------------

rf     = 0.005  # Risk-free rate
a      = 0.995  # Confidence level
cv     = 0.25   # The threshold for the additional risk measure applied
lc     = "CVaR" # The additional risk measure applied for optimizing the PF
t.cost = 0.01   # Transaction costs for rebalancing the portfolio

# -------------------- 4. Functions -------------------------------------------
# 4.1 Definition --------------------------------------------------------------

# Cal. the arithmetic return for a portfolio incl. a risk free asset
# Input returns need to be arithmetic 
PFER = function(data = NA, weights, rfr = 0) {
  if (length(weights) != ncol(data)) {
    stop("Error: Dimensions issue")
  } else {
    PFer = (1 - sum(weights)) * rfr * (nrow(data) / 260) + 
           (sapply(1:ncol(data), function(x) prod(1 + 
           data[, x])) - 1) %*% weights
    return(PFer)
  }  
}

# This function calculates the portfolio's standard deviation
# Input returns need to be arithmetic 
PFSD = function(data, weights) {
  if (length(weights) != ncol(data)) {
    stop("Error: Dimensions issue")
  } else {  
    sig2 = t(weights) %*% (cov(data) * nrow(data)) %*% weights
    return(sqrt(sig2))
  }
}

# Calculating the minimum variance portfolio (no risk free asset)
# Input returns need to be arithmetic 
wmv = function(data = NA) {
  E    = cov(data) * nrow(data)
  e    = rep(1, length(E[1, ]))
  test = try(solve(E), silent = T) # test invertability of E
  if (is(test, "try-error")) {
    stop("cov(data) cannot be inverted")
  } else {
    w        = (solve(E) %*% e) / (as.numeric(t(e) %*% solve(E) %*% e))
    wmv.er   = PFER(data, w)
    wmv.sd   = PFSD(data, w)
    r        = list(w, as.numeric(wmv.er), as.numeric(wmv.sd))
    names(r) = c("Portfolio Weights:", "Expected Return:", 
                 "Standard Deviation:")
    return(r)
  }
}

# Calculate the efficient portfolio incl. a risk free asset given a wished mean
# Input returns need to be arithmetic 
wts = function(wmu = NA, rfr = 0.005, data = NA, smean = sapply(1:ncol(data), 
               function(x) prod(1 + data[, x])) - 1) {
  E    = cov(data) * nrow(data)
  test = try(solve(E), silent = T) # test invertability of E
  if (is(test, "try-error")) {
    stop("cov(data) cannot be inverted")
  } else {
    e        = rep(1, length(smean))
    w1       = as.numeric((wmu - rf) / (t(smean - rf * e) %*% solve(E) %*% 
               (smean - rf * e)))
    w2       = solve(E) %*% (smean - rf * e)
    wts      = w1 * w2
    wts.er   = PFER(data, wts, rf)
    wts.sd   = PFSD(data, wts)
    rf.ratio = 1 - sum(wts)
    r        = list(wts, as.numeric(wts.er), as.numeric(wts.sd), rf.ratio)
    names(r) = c("Portfolio Weights:", "Expected Return:", 
                 "Standard Deviation:", "Risk-Free Ratio:")
    return(r)
  }
}

# Calculation of the tangent portfolio
# Input returns need to be arithmetic
wtpf = function(data = NA, rfr = 0.005, 
                smean = sapply(1:ncol(data), 
                               function(x) prod(1 + data[, x])) - 1) {
  E    = cov(data) * nrow(data)
  test = try(solve(E), silent = T) # test invertability of E
  if (is(test, "try-error")) {
    stop("cov(data) cannot be inverted")
  } else {
    e        = rep(1, length(smean))
    w1       = solve(E) %*% (smean - rfr * e)
    w2       = as.numeric(1 / (t(e) %*% solve(E) %*% (smean - rfr * e)))
    wt       = w1 * w2
    wt.er    = PFER(data, wt, rfr)
    wt.sd    = PFSD(data, wt)
    rf.ratio = 1 - sum(wt)
    r        = list(wt, as.numeric(wt.er), as.numeric(wt.sd), rf.ratio)
    names(r) = c("Portfolio Weights:", "Expected Return:", 
                 "Standard Deviation:", "Risk-Free Ratio:")
    return(r)
  }
}

# Efficient Frontier without a risk free asset
# Input returns need to be arithmetic
wef = function(wmu = NA, data = NA, 
               smean = sapply(1:ncol(data), 
                              function(x) prod(1 + data[, x])) - 1) {
  E    = cov(data) * nrow(data)
  test = try(solve(E), silent = T) # test invertability of E
  if (is(test, "try-error")) {
    stop("cov(data) cannot be inverted")
  } else {
    e        = rep(1, length(smean))
    M        = cbind(smean, e)
    B        = t(M) %*% solve(E) %*% M
    mp       = c(wmu, 1)
    w        = solve(E) %*% M %*% solve(B) %*% mp
    w.er     = PFER(data, w)
    w.sd     = PFSD(data, w)
    r        = list(w, as.numeric(w.er), as.numeric(w.sd))
    names(r) = c("Portfolio Weights:", "Expected Return:", 
                 "Standard Deviation:")
    return(r)
  }
}

# Value-at-Risk-Constraint
VaR.C = function(sig = NA, alpha = 0.999, V = NA) {
  if (alpha <= 0.5) {
    return(print("Error: Alpha has to be set between 0.5 and 1"))
  }
  VC = -1 * qnorm(1 - alpha) * sig - V
  return(VC)
}

# Conditional-Value-at-Risk-Constraint
CVaR.C = function(sig = NA, alpha = 0.999, CV = NA) {
  if (alpha <= 0.5) {
    return(print("Error: Alpha has to be set between 0.5 and 1"))
  }
  CVARC = -1 * (integrate(function(x) x * dnorm(x), 
                          -Inf, qnorm(1 - alpha))$value / 
          (1 - alpha)) * sig - CV
  return(CVARC)
}

# Intersection between eff. frontier incl. a risk free asset 
# and linear constraint
# Input returns need to be arithmetic
LCon.IS = function(LC = "CVaR", data = NA, rfr = 0.005, a = 0.995, 
                   cv = 0.25) {
  if (LC == "CVaR") {
    cvar           = matrix(c(-1 * (integrate(function(x) x * dnorm(x), 
                                              -Inf, qnorm(1 - a))$value / 
                            (1 - a)), -1, -(0.05 - 0.03) / 
                              (as.numeric(wts(0.05, rfr, data)[3]) - 
                            as.numeric(wts(0.03, rfr, data)[3])), 1),
                            byrow = TRUE, nrow = 2)
    cvar.sol       = c(cv, rfr)
    cvar.is        = solve(cvar, cvar.sol)
    names(cvar.is) = c("SD", "ER")
    return(cvar.is)
  }
  if (LC == "VaR") {
    var           = matrix(c(-1 * qnorm(1 - a), -1, -(0.05 - 0.03) / 
                               (as.numeric(wts(0.05, rfr, data)[3]) - 
                           as.numeric(wts(0.03, rfr, data)[3])), 1), 
                           byrow = TRUE, nrow = 2)
    var.sol       = c(cv, rfr)
    var.is        = solve(var, var.sol)
    names(var.is) = c("SD", "ER")
    return(var.is)
  } else {
    print("Error: Parameter LC must be VaR or CVaR")
  }
}

# Optimizes a given portfolio
# Input returns need to be arithmetic
PF.Opt = function(Start.PF.mu = NA, data = NA, risk.m = "VaR", rfr = 0.005, 
                  a = 0.995, cv = 0.25) {
  if (!is.numeric(Start.PF.mu)) {
    stop("Error: Start.PF.mu must be numeric")
  } else {
    opf = wts(wmu = Start.PF.mu, rfr, data = data)
    # Optimize portfolio without linear constraint
    if (opf[2] < 0) {
      opf = wts(wmu = PFER(data, weights = -1 * unlist(opf[1], 
                                                       use.names = FALSE), 
                rfr), rfr, data = data)
    }
    # Check linear constraint
    if (LCon.IS(LC = risk.m, data = data, rfr = rfr, a = a, cv = cv)[1] >= 0) {
      if (LCon.IS(LC = risk.m, data = data, rfr = rfr, a = a, cv = cv)[1] < 
          as.numeric(opf[3])) {
        opf = wts(wmu = LCon.IS(LC = risk.m, data = data, rfr = rfr, a = a, 
                  cv = cv)[2], rfr, data = data)
      }
    }
    return(opf)
  }
}

# Calculates a portfolio's value and the new weights over time
# Input returns need to be arithmetic
PF_NLV_W = function(start.weights = NA, data = NA, 
                    rfr = NA, reb = TRUE, t.cost = 0.01) {
  
  if (length(start.weights) != ncol(data)) {
    stop("Error: Dimensions issue")
  } else {
    rf.ratio = (1 - sum(as.numeric(start.weights)))
    
    if (reb == TRUE) {
      rebalanceBins = ifelse(length(which(grepl("01-02", rownames(data)))) == 0 | 
                             which(grepl("01-02", rownames(data))) %in% c(1, 2), 
                             which(grepl("01-04", rownames(data))), 
                             which(grepl("01-02", rownames(data))))
    } else {
      rebalanceBins = 0
    }
    
    data           = as.matrix(data)
    pval           = vector()           # Vector of portfolios NLV
    pval.p         = vector()           # Vector of risky assets portfolio NLVs
    pval[1]        = 1                  # Initial portfolio NLV
    pval.p[1]      = sum(start.weights) # Initial risky asset portfolio NLV
    pfrt           = vector()           # Vector of portfolio returns
    weight         = list()             # List containing weights over time
    weights        = start.weights / sum(start.weights)
    weight[[1]]    = weights            # Initial portfolio weights in percent
    weights.m      = list()             # Weights in the means of invested money
    weights.m[[1]] = start.weights      # Initial portfolio weights in money
    
    for (i in 1:nrow(data)) {
      pfrt[i]       = data[i, ] %*% weights  # stock portfolio returns
      pval.p[i + 1] = pval.p[i] * (1 + pfrt[i])
      pval[i + 1]   = pval.p[i + 1] + rf.ratio * (1 + rfr * ((i + 1) / 260))
      
      if (i %in% rebalanceBins) {
        pval.p[i + 1]      = pval.p[i + 1] * (1 - t.cost)
        pval[i + 1]        = pval.p[i + 1] + 
                             rf.ratio * (1 + rfr * ((i + 1) / 260)) * 
                             (1 - t.cost)
        weights            = start.weights / sum(start.weights)
        weights.m[[i + 1]] = pval.p[i + 1] * weights
      } else {
        weights.m[[i + 1]] = weights.m[[i]] * (1 + data[i, ])
        weights            = weights.m[[i + 1]] / pval.p[i + 1]
      }
      weight[[i + 1]] = weights
    }
    
    dw           = t(data.frame(matrix(unlist(weight), 
                                       nrow = length(unlist(weight[1])))))
    rownames(dw) = c(as.character(as.Date(rownames(data)[1]) - 1), 
                     rownames(data))
    colnames(dw) = colnames(data)
    
    ret        = list(pval, dw)
    names(ret) = c("NLV", "Weights")
    return(ret)
  }  
}

# Calculate the Sharpe-Ratio of a Strategy
# Uses a time series of strategies NLV to calculate Sharpe-Ratio
Sharpe = function(ts = NA,rfr = 0) {
  if (!is.numeric(rfr)) {
    stop("Error: rfr needs to be numeric")
  } else if (!is.vector(ts,mode="numeric")) {
    stop("Error: ts needs to be a numeric vector")
  } else {
    mean = (prod(1 + (diff(ts) / ts[-length(ts)])) - 1)
    rfr  = rfr * (length(diff(ts) / ts[-length(ts)]) / 260)
    sd   = sd(diff(ts) / ts[-length(ts)]) * sqrt(length(ts))
    return((mean - rfr) / sd)
  }
}

# ----------------------- 5. Data Seperation ----------------------------------

# Extract the first year, e.g. 2013
First_Year = min(unique(format(as.Date(rownames(stock_data$Stock_Returns)), 
                               "%Y")))

# Save returns of the first year in data.frame
Return_First_Year = exp(stock_data$Stock_Returns[format(as.Date(
  rownames(stock_data$Stock_Returns)), "%Y") == First_Year, ]) - 1

# Return of the other years
Return_Other = exp(stock_data$Stock_Returns[!format(as.Date(
  rownames(stock_data$Stock_Returns)), "%Y") == First_Year, ]) - 1

# Initial portfolio weights
wMPF = stock_data$Weights[1, ]

# ----------------------- 6. Calculations -------------------------------------

# Calculate the efficient frontier incl. a risk free asset
w.rf.eff.front  = lapply(seq(-1, 1, 0.001),
                         function(x) wts(x, 
                                         data = Return_First_Year, 
                                         rfr = rf)[1])  
er.rf.eff.front = as.numeric(sapply(seq(-1, 1, 0.001), 
                                    function(x) wts(x, 
                                                    data = Return_First_Year, 
                                                    rfr = rf)[2]))
sd.rf.eff.front = as.numeric(sapply(seq(-1, 1, 0.001), 
                                    function(x) wts(x, 
                                                    data = Return_First_Year, 
                                                    rfr = rf)[3]))

# Calculate the efficient frontier without a risk free asset
er.eff.front = as.numeric(sapply(seq(-1, 1, 0.001), 
                                 function(x) wef(x, 
                                                 data = Return_First_Year)[2]))
sd.eff.front = as.numeric(sapply(seq(-1, 1, 0.001), 
                                 function(x) wef(x, 
                                                 data = Return_First_Year)[3]))

# Calculate the Minimum-Variance-Portfolio
MVP           = data.frame(SD = wmv(Return_First_Year)[3], 
                           ER = wmv(Return_First_Year)[2])
colnames(MVP) = c("SD", "ER")

# Calculate the Tangent-Portfolio
TP           = data.frame(SD = wtpf(Return_First_Year, rfr = rf)[3], 
                          ER = wtpf(Return_First_Year, rfr = rf)[2])
colnames(TP) = c("SD", "ER")

# Calculate MPF
MPF           = data.frame(SD = PFSD(Return_First_Year, wMPF), 
                           ER = PFER(Return_First_Year, wMPF, rfr = rf))
colnames(MPF) = c("SD", "ER")

# Calculate VaR-constraint
VAR           = data.frame(SD = seq(0, 
                                    as.numeric(wts(1, rf, Return_First_Year)[[3]]), 
                                    0.001), 
                           ER = as.numeric(sapply(seq(0, 
                                                      as.numeric(wts(1, 
                                                                     rf, 
                                                                     Return_First_Year)[[3]]),
                                                      0.001), 
                           function(x) VaR.C(sig = x, alpha = a, V = cv))))
colnames(VAR) = c("SD", "ER")

# Calculate CVaR-constraint
CVAR          = data.frame(SD = seq(0, 
                                    as.numeric(wts(1, rf, Return_First_Year)[[3]]), 
                                    0.001), 
                           ER = as.numeric(sapply(seq(0, 
                                                      as.numeric(wts(1, 
                                                                     rf,
                                                                     Return_First_Year)[[3]]), 
                                                      0.001), 
                           function(x) CVaR.C(sig = x, alpha = a, CV = cv))))
colnames(VAR) = c("SD", "ER")

# ----------------------- 7. Optimize given portfolio -------------------------

# Optimal portfolio
OPF           = data.frame(SD = PF.Opt(Start.PF.mu = as.numeric(MPF[2]), 
                           data = Return_First_Year, 
                           risk.m = lc, rfr = rf, a = a, cv = cv)[3], 
                           ER = PF.Opt(Start.PF.mu = as.numeric(MPF[2]), 
                                       data = Return_First_Year, 
                           risk.m = lc, rfr = rf, a = a, cv = cv)[2])
colnames(OPF) = c("SD", "ER")

#  8. Calculate portfolio over time, prepare graphics & calculate Sharpe-Ratio

# Portfolio without optimization and without rebalancing
NormPF = PF_NLV_W(start.weights = wMPF, 
                  data = Return_Other, rfr = rf, 
                  reb = FALSE, t.cost = 0)

# Prepare data for Weights Plot
NormPF_1           = melt(NormPF$Weights)
colnames(NormPF_1) = c("Date", "Symbol", "Weights")
NormPF_1$Date      = as.Date(NormPF_1$Date)

# Portfolio without optimization and with annual rebalancing
RegularPF = PF_NLV_W(start.weights = wMPF, data = Return_Other, 
                     rfr = rf, reb = TRUE, t.cost = t.cost)

# Prepare data for Weights plot
RegularPF_1           = melt(RegularPF$Weights)
colnames(RegularPF_1) = c("Date", "Symbol", "Weights")
RegularPF_1$Date      = as.Date(RegularPF_1$Date)

# Portfolio with optimization and with annual rebalancing
OptPF = PF_NLV_W(start.weights = 
                   as.numeric(unlist(PF.Opt(Start.PF.mu = as.numeric(MPF[2]), 
                                            data = Return_First_Year, 
                                            risk.m = lc, rfr = rf)[1])),
                 data = Return_Other, rfr = rf, reb = TRUE, t.cost = t.cost)

# Prepare data for Weights plot
OptPF_1           = melt(OptPF$Weights)
colnames(OptPF_1) = c("Date", "Symbol", "Weights")
OptPF_1$Date      = as.Date(OptPF_1$Date)

# Combine strategies into one data frame
Overall.Val = data.frame("NormPF" = NormPF$NLV, 
                         "RegularPF" = RegularPF$NLV, 
                         "OptPF" = OptPF$NLV)

# Calculate ER, SD & Sharpe Ratio of the three strategies
sapply(list(OptPF$NLV, NormPF$NLV, RegularPF$NLV), 
       function(x) (prod(1 + (diff(x) / x[-length(x)])) - 1)) # ER
sapply(list(OptPF$NLV, NormPF$NLV, RegularPF$NLV), 
       function(x) sd(diff(x) / x[-length(x)]) * sqrt(length(x))) # SD
sapply(list(OptPF$NLV, NormPF$NLV, RegularPF$NLV), 
       function(x) Sharpe(ts = x, rfr = rf)) # Sharpe-Ratio


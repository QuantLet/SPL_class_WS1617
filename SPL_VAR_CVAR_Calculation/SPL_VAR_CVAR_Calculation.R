# ------------------------------------------------------------------------------
# Quantlet No. 2 : VaR and CVaR Quantlet
#
# This quantlet calculates both the Value at Risk and Conditional Value at Risk 
# using three different methods, namely Historical Simulation, Analytical Models 
# and Monte Carlo Simulation.
# ------------------------------------------------------------------------------

# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Source Data Preparation Quantlet
source("Quantlet1/Data_Preparation_Quantlet.R")

# Install and load packages
libraries = c("ggplot2", "reshape2", "stats", "e1071")
lapply(libraries, function(x) 
    if (!(x %in% installed.packages())) {
        install.packages(x)
    }
)
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# -----------------------------------------------------------------------------
#  Setting of parameters 
# -----------------------------------------------------------------------------

alpha            = 0.95                # Confidence level
input.pf         = stock_data$Returns  # Data input: vector or dataframe 
                                       # with one column
weight           = 0.995               # Weighting of observations
DoF              = 4                   # Degrees of freedom
start.stockPrize = 9000                # Start stock prize for Geometric 
                                       # Brownian Motion (GBM) in MC simulation
MC.size          = 10000               # Sample size of Monte Carlo sample
num.randWalk     = 100                 # Number of random walks per path for 
                                       # GBM 
# -----------------------------------------------------------------------------
#  Calculation of VaR and CVaR using Historical Simulation 
#  with & without weighting 
# -----------------------------------------------------------------------------

# Define function for calculating VaR and CVaR using Historical Simulation 
# Arguments: a (confidence level), input.pf (historical portfolio data - 
# log-returns), risk.measure (VaR or CVaR), lambda (weight of observations) 
# Output: Vector containing VaR or CVaR with and without weighting
Hist_Sim = function(a = 0.95, input.pf, risk.measure, lambda = 0.995) {
    
    # Print error message and stop calculation if input.pf is not
    # specified
    if (missing(input.pf)) {
      stop("Need to specify input.pf for calculations.")
      
    # Convert vector to dataframe  
    } else if (is.vector(input.pf)) {
      input.pf = as.data.frame(input.pf)
    }
    # Print error message and stop calculation if input.pf is not
    # correctly specified
    if (dim(input.pf)[2] > 1) {
      stop("input.pf has to be a vector or dataframe with one column")
    }
    # Rename column with returns 
    colnames(input.pf) = "Returns"
    
    if (!is.numeric(lambda) | lambda <= 0 | lambda >= 1) {
        # Print error message and stop calculation if lambda is not
        # correctly specified
        stop("Need to specifiy 0 < lambda < 1 for calculations.")
    } else if (!is.numeric(a) | a <= 0 | a >= 1) {
        # Print error message and stop calculation if a is not correctly
        # specified
        stop("Need to specifiy 0 < a < 1 for calculations.")
    } else if (a > 0.5) {
        # Transform a for calculations
        a = 1 - a
    }
    
    # Print error message and stop calculation if risk.measure is not
    # specified
    if (missing(risk.measure)) {
        stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
              calculations.")
        
    # Calculate VaR using Historical Simulation
    } else if (risk.measure == "VaR") {
        
        # Calculate VaR using Historical Simulation with equal weights for
        # each observation
        VaR_hist = quantile(input.pf$Returns, a, names = FALSE)
        
        # Calculate VaR using historical simulation with higher weights for
        # more recent observations
        w_input.pf         = input.pf
        
        # Calculate respective weights for observations
        w_input.pf$Weights = (lambda^(nrow(w_input.pf) - (1:nrow(w_input.pf))) * 
                             (1 - lambda)) / (1 - lambda^nrow(w_input.pf))   
        
        # Order observations with respect to log returns
        w_input.pf         = w_input.pf[order(w_input.pf$Returns), ]
        
        # Sum weights until required percentile of distribution is reached: 
        # Respective log return is VaR
        w_VaR_hist         = min(w_input.pf$Returns[cumsum(w_input.pf$Weights) > a])  
        
        return(c(VaR_hist, w_VaR_hist))
        
    # Calculate CVaR using Historical Simulation
    } else if (risk.measure == "CVaR") {
        
        # Calculate VaR using Historical Simulation
        VaR_vec = Hist_Sim(a = a, input.pf = input.pf, risk.measure = "VaR", 
                  lambda = lambda)
        
        # Calculate CVaR using Historical Simulation with equal weights for
        # each observation
        CVaR_hist = mean(input.pf$Returns[input.pf$Returns <= VaR_vec[1]])
        
        # Calculate VaR using Historical Simulation with higher weights for
        # more recent observations
        w_CVaR_hist = mean(input.pf$Returns[input.pf$Returns <= VaR_vec[2]])
        
        return(c(CVaR_hist, w_CVaR_hist))
        
    # Print error message and stop calculation if risk.measure is not
    # correctly specified
    } else {
        stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
              calculations.")
    }
}

# -----------------------------------------------------------------------------
#  Calculation of VaR and CVaR using analytical models 
# -----------------------------------------------------------------------------

# Define function for calculating VaR and CVaR using analytical models 
# Arguments: a (confidence level), input.pf (historical portfolio data - 
# log returns), risk.measure (VaR or CVaR), df (degrees of freedom) 
# Output: Vector containing VaR or CVaR under normal and Student's t-distribution 
# and Cornish-Fisher Expansion
Analyt_Mod = function(a = 0.95, input.pf, risk.measure, df) {
    
    # Print error message and stop calculation if input.pf is not
    # specified
    if (missing(input.pf)) {
        stop("Need to specify input.pf for calculations.")
      
    # Convert vector to dataframe  
    } else if (is.vector(input.pf)) {
        input.pf = as.data.frame(input.pf)
    }
    # Print error message and stop calculation if input.pf is not
    # correctly specified
    if (dim(input.pf)[2] > 1) {
        stop("input.pf has to be a vector or dataframe with one column")
    }
    # Rename column with returns 
    colnames(input.pf) = "Returns"
    
    if (missing(df) | !is.numeric(df) | df <= 0) {
        # Print error message and stop calculation if df is not specified
        stop("Need to specify df > 0 for calculations.")
    } else if (!is.numeric(a) | a <= 0 | a >= 1) {
        # Print error message and stop calculation if a is not correctly
        # specified
        stop("Need to specifiy 0 < a < 1 for calculations.")
    } else if (a > 0.5) {
        # Transform a for calculations
        a = 1 - a
    }
    
    # Estimate standard deviation and mean from historical data
    sd_pf   = sd(input.pf$Returns)    
    mean_pf = mean(input.pf$Returns)  
  
    # Print error message and stop calculation if risk.measure is not specified
    if (missing(risk.measure)) {
        stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
              calculations.")
        
    # Calculate VaR using analytical models
    } else if (risk.measure == "VaR") {
        
        # Calculate VaR under assumption of normal distribution
        n_VaR = qnorm(a, 0, 1) * sd_pf + mean_pf  # VaR
        
        # Calculate VaR under assumption of Student's t-distribution
        t_VaR = qt(a, df) * sd_pf * sqrt((df - 2) / df) + mean_pf
        
        # Calculate VaR with Cornish-Fisher Expansion
        z        = qnorm(a, 0, 1)
        skew     = skewness(input.pf$Returns)
        kurt     = kurtosis(input.pf$Returns)
        cf_VaR   = mean_pf + (z + (1 / 6) * (z^2 - 1) * skew + 
                   (1 / 24) * (z^3 - 3 * z) * (kurt - 3) - 
                   (1 / 36) * (2 * z^3 - 5 * z) * skew^2) * sd_pf
        
        return(c(n_VaR, t_VaR, cf_VaR))
        
    # Calculate CVaR using analytical models
    } else if (risk.measure == "CVaR") {
        
        # Calculate VaR using analytical models
        VaR_vec = Analyt_Mod(a = a, input.pf = input.pf, risk.measure = "VaR", 
                             df = df)
        
        # Calculate CVaR under assumption of normal distribution
        n_CVaR = integrate(function(x) x * dnorm(x, mean_pf, sd_pf),
                           -Inf, VaR_vec[1])$value / a
        
        # Calculate CVaR under assumption of Student's t-distribution
        t_CVaR = - mean_pf - sd_pf * sqrt((df - 2) / df) * dt(qt(a, df), df) / a * 
                 (df + qt(a, df)^2) / (df - 1)
        
        # Calculate CVaR with Cornish-Fisher Expansion
        skew     = skewness(input.pf$Returns)
        kurt     = kurtosis(input.pf$Returns)
        cf_CVaR  = integrate(function(x) (mean_pf + (qnorm(x, 0, 1) + (1 / 6) 
                   * (qnorm(x, 0, 1)^2 - 1) * skew + (1 / 24) * (qnorm(x, 0, 1)^3 
                   - 3 * qnorm(x, 0, 1)) * kurt - (1 / 36) * (2 * 
                   qnorm(x, 0, 1)^3 - 5 * qnorm(x, 0, 1)) * skew^2) * 
                   sd_pf), 0, a)$value / a
        
        return(c(n_CVaR, t_CVaR, cf_CVaR))   
    # Print error message and stop calculation if risk.measure is not correctly 
    # specified
    } else {
        stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
              calculations.")
    }
}

# -----------------------------------------------------------------------------
#  Calculation of VaR and CVaR using Monte Carlo Simulation
# -----------------------------------------------------------------------------

# Define function for calculating VaR and CVaR using Monte Carlo Simulation
# Arguments: a (confidence level), input.pf (historical portfolio data - log 
# returns), risk.measure (VaR or CVaR), S0 (start stock prize for Geometric 
# Brownian motion (GBM)), sample.size (sample size of simulated sample), 
# num.randWalk (number of random walks within one simulated path in GBM), 
# df (degrees of freedom)
# Output: Vector containing VaR or CVaR for assumption of normal and of 
# Student's t-distribution
MonteCarlo = function(a = 0.05, input.pf = NA, risk.measure = NA, S0 = 9000,
                      sample.size = 10000, num.randWalk = 100, df) {
    # Print error message and stop calculation if input.pf is not
    # specified
    if (missing(input.pf)) {
        stop("Need to specify input.pf for calculations.")
      
    # Convert vector to dataframe  
    } else if (is.vector(input.pf)) {
        input.pf = as.data.frame(input.pf)
    }
    # Print error message and stop calculation if input.pf is not
    # correctly specified
    if (dim(input.pf)[2] > 1) {
        stop("input.pf has to be a vector or dataframe with one column")
    }
    # Rename column with returns 
    colnames(input.pf) = "Returns"
    
    if (missing(df) | !is.numeric(df) | df <= 0) {
        # Print error message and stop calculation if df is not specified
        stop("Need to specify df > 0 for calculations.")
    } else if (!is.numeric(S0) | S0 <= 0) {
        # Print error message and stop calculation if a or df are
        # not numeric
        stop("S0 has to be a numeric value > 0 for calculations.")
    } else if (!is.numeric(sample.size) | sample.size <= 0) {
        # Print error message and stop calculation if S0, sample.size
        # or num.randWalk are negative or equal to zero
        stop("sample.size has to be a numeric value > 0 for calculations.")
    } else if (!is.numeric(num.randWalk) | num.randWalk <= 0) {
        # Print error message and stop calculation if df is not correctly  
        # specified
        stop("num.randWalk has to be a numeric value > 0 for calculations.")
    } else if (!is.numeric(a) | a <= 0 | a >= 1) {
        # Print error message and stop calculation if a is not correctly
        # specified
        stop("Need to specifiy 0 < a < 1 for calculations.")
    } else if (a > 0.5) {
        # Transform a for calculations
        a = 1 - a
    }
  
    # Determine mean and standard deviation of given data
    mean_pf = mean(input.pf$Returns)
    sd_pf   = sqrt(var(input.pf$Returns))  
    
    # Create a matrix of standard normally distributed random numbers
    Z.rv = matrix(rnorm((num.randWalk - 1) * sample.size, 0, 1), 
                  nrow = num.randWalk - 1)
    
    # Specify deltat, start stock prize and define matrix for GBM stock prizes
    dt              = 1 / num.randWalk
    start.val       = S0  
    stock.data      = matrix(nrow = num.randWalk, ncol = sample.size)
    stock.data[1, ] = start.val
    
    # Simulate stock prizes using Geometric Brownian motion
    for (i in 1:(num.randWalk - 1)) {
      stock.data[i + 1, ] = stock.data[i, ] * exp(sd_pf * sqrt(dt) * Z.rv[i, ] 
                                                  + mean_pf * dt)        
    }
    
    # Save only last row of the data set for calculation of risk measures
    # and transform stock prizes to log-returns
    MC_data_norm = log(stock.data[nrow(stock.data), ]) - log(S0)
  
    # Simulate data with Student's t-distribution 
    MC_data_t = rt(sample.size, df) * sd_pf * sqrt((df - 2) / df) + mean_pf
  
  # Print error message and stop calculation if risk.measure is not specified
  if (missing(risk.measure)) {
      stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
           calculations.")
    
  # Calculation of VaR
  } else if (risk.measure == "VaR") {
      MC_VaR_norm = quantile(sort(MC_data_norm), a, names = FALSE)
      MC_VaR_t    = quantile(sort(MC_data_t), a, names = FALSE)
      
      return(list("MC_data_norm" = MC_data_norm, "MC_VaR_norm" = MC_VaR_norm,
                  "MC_data_t" = MC_data_t, "MC_VaR_t" = MC_VaR_t))
    
  # Calculation of CVaR
  } else if (risk.measure == "CVaR") {
      VaR_norm     = quantile(sort(MC_data_norm), a, names = FALSE)
      VaR_t        = quantile(sort(MC_data_t), a, names = FALSE)
      MC_CVaR_norm = mean(MC_data_norm[MC_data_norm <= VaR_norm])
      MC_CVaR_t    = mean(MC_data_t[MC_data_t <= VaR_t])
      
      return(list("MC_data_norm" = MC_data_norm, "MC_CVaR_norm" = MC_CVaR_norm,
                  "MC_data_t" = MC_data_t, "MC_CVaR_t" = MC_CVaR_t))
  
  # Print error message and stop calculation if risk.measure is not correctly 
  # specified
  } else {
      stop("Need to specify risk.measure ('VaR' or 'CVaR') for 
            calculations.")
  }
}

# -----------------------------------------------------------------------------
#  Function Calls
# -----------------------------------------------------------------------------
# Run Hist_Sim function for VaR and CVaR
Hist.VaR  = Hist_Sim(a = alpha, input.pf = input.pf, risk.measure = "VaR", 
                     lambda = weight)
Hist.CVaR = Hist_Sim(a = alpha, input.pf = input.pf, risk.measure = "CVaR", 
                     lambda = weight)

# Run Analyt_Mod function for VaR and CVaR
Analyt.VaR  = Analyt_Mod(a = alpha, input.pf = input.pf, risk.measure = "VaR", 
                         df = DoF)
Analyt.CVaR = Analyt_Mod(a = alpha, input.pf = input.pf, risk.measure = "CVaR", 
                         df = DoF)

# Run MonteCarlo function for VaR and CVaR
set.seed(333)
MC.VaR  = MonteCarlo(a = alpha, input.pf = input.pf, risk.measure = "VaR",
                     S0 = start.stockPrize, sample.size = MC.size, 
                     num.randWalk = num.randWalk, df = DoF)
set.seed(333)
MC.CVaR = MonteCarlo(a = alpha, input.pf = input.pf, risk.measure = "CVaR",
                     S0 = start.stockPrize, sample.size = MC.size, 
                     num.randWalk = num.randWalk, df = DoF)



